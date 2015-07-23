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
		constexpr typename std::enable_if<std::is_integral<T>::value, unsigned int>::type
			log2_floor(T value)
		{
			return (value <= 1) ? 0 : (1 + log2_floor(value >> 1));
		}

		template<typename T>
		constexpr typename std::enable_if<std::is_integral<T>::value, unsigned int>::type
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
		constexpr typename std::enable_if<std::is_floating_point<T>::value, unsigned int>::type
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
		constexpr typename std::enable_if<std::is_floating_point<T>::value, unsigned int>::type
			log2_ceil(T value)
		{
			return log2_ceil(largest_unsigned_type(ceil(value)));
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

		template<typename T>
		constexpr T min(T lhs, T rhs)
		{
			return (lhs > rhs) ? rhs : lhs;
		}

		constexpr int binary_digits(double decimal)
		{
			return static_cast<int>(ceil(3.3219280948873623478703194294894 * decimal));
		}

		template<typename T>
		constexpr T bitmask(int bits)
		{
			return (bits < sizeof(T) * 8) ? ((T(1) << bits) - 1) : T(-1);
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

		template< typename T, RoundModes Mode = RoundModes::Floor >
		constexpr typename std::enable_if<std::is_floating_point<T>::value, T>::type
			scaled_exp2(T value, int exponent)
		{
			return ((exponent >= 0) ? detail::floating_round_switch<Mode>::round(value * exp2<T>(exponent)) : (value / exp2<T>(-exponent)));
		}

		template< typename T, RoundModes Mode = RoundModes::Floor >
		constexpr typename std::enable_if<std::is_integral<T>::value, T>::type
			scaled_exp2(T value, int exponent)
		{
			return (exponent >= 0) ?
				(value << exponent)
				:
				(detail::integral_round_switch<Mode>::round(value, -exponent) >> (-exponent));
		}

		//
		// Supporting Macros
		//
		template<typename S>
		constexpr unsigned int integer_bits(S value)
		{
			//     simple log2 of the integer part     if signed we need a bit more         if value is positive and matches a power, we need one more again
			return log2_ceil(trunc(abs(value))) + ((value < 0) ? 1 : 0) + ((value > 0 && is_power_of_2(trunc(value))) ? 1 : 0);
		}

		template<typename S, typename U>
		constexpr unsigned int integer_bits_interval(S s, U u)
		{
			// FIXME: sign handling is no correct
			return util::max(integer_bits(s), integer_bits(u)) + ((s < 0 || u <0) ? 1 : 0);
		}

	}

}

#endif
