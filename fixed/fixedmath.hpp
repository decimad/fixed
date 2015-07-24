//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef UTIL_FIXEDMATH_HPP__
#define UTIL_FIXEDMATH_HPP__

#include <limits>

namespace fix {

	namespace util {

		using largest_unsigned_type = unsigned long long;
		using largest_signed_type   = long long;

		template<typename T>
		constexpr typename std::enable_if<std::is_integral<T>::value, T>::type
			exp2(unsigned int exponent)
		{
			return T(1) << exponent;
		}

		template<typename T>
		constexpr typename std::enable_if<std::is_floating_point<T>::value, T>::type
			exp2(unsigned int exponent)
		{
			return (exponent == 0) ? 1 : (exp2<T>(exponent - 1) * 2);
		}

		template< typename T >
		constexpr T shifted(T value, int amount)
		{
			return (amount >= 0) ? (value << amount) : (value >> (-amount));
		}

		template< typename T >
		constexpr T sign(T val)
		{
			return (val > 0) ? 1 : ((val < 0) ? -1 : 0);
		}

		template< typename T>
		constexpr typename std::enable_if<std::is_integral<T>::value, int>::type
			log2_floor(T value)
		{
			return (value <= 1) ? 0 : (1 + log2_floor(value >> 1));
		}

		template<typename T>
		constexpr typename std::enable_if<std::is_integral<T>::value, int>::type
			log2_ceil(T value)
		{
			return
				(value <= 1) ? 0 :
				(((value & 1) && (value > 1)) ? (2 + log2_floor(value >> 1)) : (1 + log2_ceil(value >> 1)));
		}

		template< typename T >
		constexpr T floor(T value)
		{
			return
				static_cast<T>(
					(value - largest_signed_type(value) == 0) ? largest_signed_type(value) :
					((value >= 0) ? largest_signed_type(value) : largest_signed_type(value) - 1)
					);
		}

		template< typename T >
		constexpr T ceil(T value)
		{
			return static_cast<T>(
				(value - largest_signed_type(value) == 0) ? int(value) :
				((value >= 0) ? (largest_signed_type(value) + 1) : largest_signed_type(value))
				);
		}

		template<typename T>
		constexpr typename std::enable_if<std::is_floating_point<T>::value, int>::type
			log2_floor(T value)
		{
			return log2_floor(largest_unsigned_type(floor(value)));
		}

		template< typename T >
		constexpr typename std::enable_if<std::is_integral<T>::value, T>::type
			round(T value)
		{
			return value;
		}

		template< typename T >
		constexpr typename std::enable_if<std::is_floating_point<T>::value, T>::type
			round(T value)
		{
			return static_cast<T>(static_cast<largest_signed_type>(value + sign(value)*T(0.5)));
		}

		template<typename T>
		constexpr typename std::enable_if<std::is_floating_point<T>::value, int>::type
			log2_ceil(T value)
		{
			return (value >= 1) ? log2_ceil(largest_unsigned_type(ceil(value))) : -log2_floor(largest_unsigned_type(floor(1.0/value)));
		}

		template< typename T >
		constexpr T clamp(T value, T min, T max)
		{
			return (value > max) ? max : ((value < min) ? min : value);
		}

		template<typename T>
		constexpr typename std::enable_if< std::is_floating_point<T>::value || std::is_signed<T>::value, T>::type
			abs(T value)
		{
			return (value >= 0) ? value : -value;
		}

		template<typename T>
		constexpr typename std::enable_if< std::is_integral<T>::value && !std::is_signed<T>::value, T>::type
			abs(T value)
		{
			return value;
		}

		template<typename T>
		constexpr typename std::enable_if<std::is_integral<T>::value && std::is_signed<T>::value, T>::type
			safe_abs(T value)
		{
			return abs<T>(clamp<T>(value, -std::numeric_limits<T>::max(), std::numeric_limits<T>::max()));
		}

		template<typename T>
		constexpr typename std::enable_if<std::is_unsigned<T>::value || std::is_floating_point<T>::value, T>::type
			safe_abs(T value)
		{
			return abs(value);
		}

		template<typename T>
		constexpr T trunc(T value)
		{
			return static_cast<T>(static_cast<largest_signed_type>((value)));
		}

		template<typename T>
		constexpr bool is_power_of_2(T value)
		{
			return exp2<T>(log2_floor(safe_abs(value))) == safe_abs(value);
		}

		template<typename T>
		constexpr T max(T lhs, T rhs)
		{
			return (lhs > rhs) ? lhs : rhs;
		}

		template<typename T, typename... Args>
		constexpr T max(T t1, T t2, T t3, Args&&... t)
		{
			return max(max(t1, t2), t3, t...);
		}

		template<typename T>
		constexpr T min(T lhs, T rhs)
		{
			return (lhs > rhs) ? rhs : lhs;
		}

		template<typename T, typename... Args>
		constexpr T min(T t1, T t2, T t3, Args... t)
		{
			return min(min(t1, t2), t3, t...);
		}

		constexpr int binary_digits(double decimal)
		{
			return static_cast<int>(ceil(3.3219280948873623478703194294894 * decimal));
		}

		namespace detail {
			template<typename T>
			constexpr T bitmask2(int bits) {
				return ((bits < sizeof(T) * 8) ? ((T(1) << bits) - 1) : T(-1));
			}
		}

		template<typename T>
		constexpr T bitmask(int bits)
		{
			// to prevent warnings
			return (bits <= 0) ? 0 : detail::bitmask2<T>(bits);
		}

		namespace detail {

			template< typename T, int Bits, bool Signed = std::is_signed<T>::value >
			struct maxbits {
				static constexpr T value = bitmask<T>(Bits);
			};

			template< typename T, int Bits >
			struct maxbits< T, Bits, true > {
				static constexpr T value = bitmask<T>(Bits - 1);
			};

		}

		template< typename T, int Bits >
		struct limits {
			static constexpr T max = detail::maxbits<T, Bits>::value;
			static constexpr T min = std::is_signed<T>::value ? (~(1 << (Bits - 1)) + 1) : 0;
		};

		template< typename T, int Bits, typename S >
		constexpr bool test_overflow(S value) {
			return sign(static_cast<T>(value)) == sign(value) && static_cast<T>(value) <= limits<T, Bits>::max && static_cast<T>(value) >= limits<T, Bits>::min;
		}

		//
		// Rounding (note I have to somehow emulate business for floats, since visual c++ doesn't define floating rounding constexpr :( )
		//          Although this can be very inefficient, since it's only ever called for constexpr calls

		enum class RoundModes {
			Floor,
			Ceil,
			Zero,
			Infinity,
			NearestOdd,
			NearestEven,
			NearestUp,
			NearestDown
		};

		template< typename T >
		constexpr typename std::enable_if< std::is_integral<T>::value, T >::type
			ceil(T value, int digits)
		{
			return (value & ~bitmask<T>(digits)) + ((value & bitmask<T>(digits)) ? (T(1) << digits) : 0);
		}

		template< typename T >
		constexpr typename std::enable_if< std::is_integral<T>::value, T >::type
			floor(T value, int digits)
		{
			return (value & ~bitmask<T>(digits));
		}

		template< typename T >
		constexpr typename std::enable_if< std::is_integral<T>::value, T >::type
			towards_zero(T value, int digits)
		{
			return (value > 0) ? floor(value, digits) : ceil(value, digits);
		}

		template< typename T >
		constexpr typename std::enable_if< std::is_floating_point<T>::value, T >::type
			towards_zero(T value)
		{
			return (value > 0) ? floor(value) : ceil(value);
		}

		template< typename T >
		constexpr typename std::enable_if< std::is_integral<T>::value, T >::type
			towards_infinity(T value, int digits)
		{
			return (value >= 0) ? ceil(value, digits) : floor(value, digits);
		}

		template< typename T >
		constexpr typename std::enable_if< std::is_floating_point<T>::value, T >::type
			towards_infinity(T value)
		{
			return (value >= 0) ? ceil(value) : floor(value);
		}

		template< typename T >
		constexpr typename std::enable_if< std::is_integral<T>::value, T >::type
			nearest_up(T value, int digits)
		{
			return ((value & bitmask<T>(digits)) >= (1 << (digits - 1))) ? ceil(value, digits) : floor(value, digits);
		}

		template< typename T >
		constexpr typename std::enable_if< std::is_floating_point<T>::value, T >::type
			nearest_up(T value)
		{
			return (value - floor(value) >= 0.5) ? ceil(value) : floor(value);
		}

		template< typename T >
		constexpr typename std::enable_if< std::is_integral<T>::value, T >::type
			nearest_down(T value, int digits)
		{
			return ((value & bitmask<T>(digits)) <= (T(1) << (digits - 1))) ? floor(value, digits) : ceil(value, digits);
		}

		template< typename T >
		constexpr typename std::enable_if< std::is_floating_point<T>::value, T >::type
			nearest_down(T value)
		{
			return (value - floor(value) <= 0.5) ? floor(value) : ceil(value);
		}

		namespace detail {
			// Could separate into Pre and Post round operations to handle floats and integrals equally,
			// but that would just mess up the code with no gains.

			// Float rounding happening in fixed<>::from only

			// Integral rounding happening throughout
			template< RoundModes Mode >
			struct integral_round_switch;

			// Truncate ... fastest rounding possible
			template<>
			struct integral_round_switch< RoundModes::Floor >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					// For now I'm relying that the compiler will remove this
					// if a shifting operation follows right after.
					return floor(value, digits);
				}
			};

			template<>
			struct integral_round_switch< RoundModes::Ceil >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return ceil(value, digits);
				}
			};

			template<>
			struct integral_round_switch< RoundModes::Zero >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return towards_zero(value, digits);
				}
			};

			template<>
			struct integral_round_switch< RoundModes::Infinity >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return towards_infinity(value, digits);
				}
			};

			template<>
			struct integral_round_switch< RoundModes::NearestUp >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return nearest_up(value, digits);
				}
			};

			template<>
			struct integral_round_switch< RoundModes::NearestDown >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return nearest_down(value, digits);
				}
			};


			// Integral rounding happening throughout
			template< RoundModes Mode >
			struct floating_round_switch;

			// Truncate ... fastest rounding possible
			template<>
			struct floating_round_switch< RoundModes::Floor >
			{
				template< typename T >
				static constexpr T round(T value)
				{
					// For now I'm relying that the compiler will remove this
					// if a shifting operation follows right after.
					return floor(value);
				}
			};

			template<>
			struct floating_round_switch< RoundModes::Ceil >
			{
				template< typename T >
				static constexpr T round(T value)
				{
					return ceil(value);
				}
			};

			template<>
			struct floating_round_switch< RoundModes::Zero >
			{
				template< typename T >
				static constexpr T round(T value)
				{
					return towards_zero(value);
				}
			};

			template<>
			struct floating_round_switch< RoundModes::Infinity >
			{
				template< typename T >
				static constexpr T round(T value)
				{
					return towards_infinity(value);
				}
			};

			template<>
			struct floating_round_switch< RoundModes::NearestUp >
			{
				template< typename T >
				static constexpr T round(T value)
				{
					return nearest_up(value);
				}
			};

			template<>
			struct floating_round_switch< RoundModes::NearestDown >
			{
				template< typename T >
				static constexpr T round(T value)
				{
					return nearest_down(value);
				}
			};

		}

		//
		// Scaling operations
		// (Fixme: is the usage of rounding here really intuitive?)
		//

		template< RoundModes Mode = RoundModes::Floor, typename T >
		constexpr typename std::enable_if<std::is_floating_point<T>::value, T>::type
			scaled_exp2(T value, int exponent)
		{
			return (exponent >= 0) ? 
				(detail::floating_round_switch<Mode>::round(value * exp2<int64>(exponent))) : (value / exp2<int64>(-exponent));
		}

		template<RoundModes Mode = RoundModes::Floor, typename T>
		constexpr typename std::enable_if<std::is_integral<T>::value, T>::type
			scaled_exp2(T value, int exponent)
		{
			return (exponent >= 0) ?
				(value << exponent)
				:
				(detail::integral_round_switch<Mode>::round(value, -exponent) >> (-exponent));
		}

		namespace detail {
		
			template< typename T, T Value, T Min, T Max, bool Signed = std::is_signed<T>::value >
			struct bits_for_value_helper
			{
				static constexpr int value = util::log2_ceil(util::abs(Value + 1));
			};

			template< typename T, T Value, T Min, bool Signed >
			struct bits_for_value_helper< T, Value, Min, Value, Signed >
			{
				static constexpr int value = sizeof(T) * 8 - (Signed ? 1 : 0);
			};

			template< typename T, T Value, T Max >
			struct bits_for_value_helper< T, Value, Value, Max, true >
			{
				static constexpr int value = sizeof(T) * 8 - 1;
			};
		
		}

		template<typename T, T Value>
		struct bits_for_value {
			static constexpr int value = detail::bits_for_value_helper<T, Value, std::numeric_limits<T>::min(), std::numeric_limits<T>::max()>::value;
		};

		template<typename T, T a, T b>
		struct bits_for_range
		{
			static constexpr T min = util::min(a, b);
			static constexpr T max = util::max(a, b);

			static constexpr int min_bits = bits_for_value<T, min>::value;
			static constexpr int max_bits = bits_for_value<T, max>::value;

			static constexpr int value = util::max(min_bits, max_bits) + ((min < 0 || max < 0) ? 1 : 0);
		};

		//
		// Supporting Macros
		//
	
		namespace detail {

			template< typename T >
			constexpr int integer_bits_inbetween(T value)
			{
				return log2_ceil(abs(value + 1)) + ((value < 0) ? 1 : 0);
			}


			template< typename T >
			constexpr int integer_bits(T value)
			{
				return (value == std::numeric_limits<T>::max() || (std::is_signed<T>::value && value == std::numeric_limits<T>::min())) ? (sizeof(T)*8) : integer_bits_inbetween(value);
			}

		}

		template<typename T>
		constexpr typename std::enable_if< std::is_integral<T>::value, int>::type
			integer_bits(T value)
		{
			// since we have to work inside the type boundaries, I found no better way.
			return (value >= 0) ? detail::integer_bits<typename std::make_unsigned<T>::type>((value)) : detail::integer_bits(value);
		}

		template<typename T>
		constexpr typename std::enable_if < std::is_floating_point<T>::value, int>::type
			integer_bits(T value)
		{
			//     simple log2 of the integer part     if signed we need a bit more         if value is positive and matches a power, we need one more again
			return integer_bits(static_cast<int64>(value));
		}

		template<typename S, typename U>
		constexpr int range_bits(S s, U u)
		{
			// FIXME: sign handling is not correct
			return ((integer_bits(s) == integer_bits(u)) && ((s<0 && u>=0)|| (s>=0 && u<0))) ? (integer_bits(s)+1) : max(integer_bits(s), integer_bits(u));
		}

	}

}

#endif
