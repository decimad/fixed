//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FIXED_FIXEDMATH_HPP__
#define FIXED_FIXEDMATH_HPP__

#include <limits>
#include <fixed/fixedutil.hpp>

namespace fix {

	namespace util {

		//
		// abs variations
		//

		using largest_unsigned_type = unsigned long long;
		using largest_signed_type   = long long;

		// abs2 will promote the type to unsigned so the resulting value fits in every case
		template<typename T>
		constexpr enable_if_integral_t<T, make_unsigned_if_integral_t<T>>
			abs2(T value)
		{
			using result_type = make_unsigned_t<T>;
			return (value >= 0) ? static_cast<result_type>(value) : (static_cast<result_type>(~value + 1u));
		}

		template<typename T>
		constexpr enable_if_floating_point_t<T, T>
			abs2(T value)
		{
			return (value >= 0) ? value : -value;
		}

		template< typename T >
		constexpr T clamp(T value, T min, T max)
		{
			return (value > max) ? max : ((value < min) ? min : value);
		}

		template<typename T>
		constexpr enable_if_t< std::is_floating_point<T>::value || std::is_signed<T>::value, T>
			abs(T value)
		{
			return (value >= 0) ? value : -value;
		}

		template<typename T>
		constexpr enable_if_t< std::is_integral<T>::value && !std::is_signed<T>::value, T>
			abs(T value)
		{
			return value;
		}

		template<typename T>
		constexpr enable_if_t<std::is_integral<T>::value && std::is_signed<T>::value, T>
			safe_abs(T value)
		{
			return abs<T>(clamp<T>(value, -std::numeric_limits<T>::max(), std::numeric_limits<T>::max()));
		}

		template<typename T>
		constexpr enable_if_t<std::is_unsigned<T>::value || std::is_floating_point<T>::value, T>
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
		constexpr enable_if_integral_t<T, T>
			exp2(unsigned int exponent)
		{
			return T(1) << exponent;
		}

		template<typename T>
		constexpr enable_if_floating_point_t<T, T>
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
		constexpr enable_if_integral_t<T, int>
			log2_floor(T value)
		{
			return (value <= 1) ? 0 : (1 + log2_floor(value >> 1));
		}

		template<typename T>
		constexpr enable_if_integral_t<T, int>
			log2_ceil(T value)
		{
			return
				(value == 1) ? 0 :
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

		namespace detail {

			template< typename T >
			constexpr int log2_floor_float(T value) {
				return 
					(value == T(2)) ? 1 :
						((value > T(2)) ? (1 + log2_floor_float(value / 2)) : 0);
			}

			template< typename T >
			constexpr int log2_ceil_float(T value) {
				return 
					(value == T(2)) ? 1 :
						((value > T(2)) ? (1 + log2_ceil_float(value / 2)) : 1);
			}

		}

		template<typename T>
		constexpr enable_if_floating_point_t< T, int >
			log2_floor(T value)
		{
			return
				(value == T(1)) ? 0 :
					((value >= T(1)) ? detail::log2_floor_float(value) : (-detail::log2_ceil_float(T(1) / value)));
		}

		template<typename T>
		constexpr enable_if_floating_point_t< T, int >
			log2_ceil(T value)
		{
			return 
				(value == T(1)) ? 0 :
					((value > T(1)) ? detail::log2_ceil_float(value) : (-detail::log2_floor_float(T(1) / value)));
		}

		template< typename T >
		constexpr enable_if_integral_t<T, T>
			round(T value)
		{
			return value;
		}

		template< typename T >
		constexpr enable_if_floating_point_t< T, T >
			round(T value)
		{
			return static_cast<T>(static_cast<largest_signed_type>(value + sign(value)*T(0.5)));
		}

		template<typename T>
		constexpr bool is_power_of_2(T value)
		{
			return exp2<T>(log2_floor(safe_abs(value))) == safe_abs(value);
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

		template< bool NoMask = false, typename T >
		constexpr enable_if_integral_t< T, T >
			ceil(T value, int digits)
		{
			return (NoMask ? (value)  : (value & ~bitmask<T>(digits))) + ((value & bitmask<T>(digits)) ? (T(1) << digits) : 0);
		}

		template< bool NoMask = false, typename T >
		constexpr enable_if_integral_t< T, T >
			floor(T value, int digits)
		{
			return NoMask ? value : (value & ~bitmask<T>(digits));
		}

		template< bool NoMask = false, typename T >
		constexpr enable_if_integral_t< T, T >
			towards_zero(T value, int digits)
		{
			return (value > 0) ? floor<NoMask>(value, digits) : ceil<NoMask>(value, digits);
		}

		template< typename T >
		constexpr enable_if_floating_point_t< T, T >
			towards_zero(T value)
		{
			return (value > 0) ? floor(value) : ceil(value);
		}

		template< bool NoMask = false, typename T >
		constexpr enable_if_integral_t< T, T >
			towards_infinity(T value, int digits)
		{
			return (value >= 0) ? ceil<NoMask>(value, digits) : floor<NoMask>(value, digits);
		}

		template< typename T >
		constexpr enable_if_floating_point_t< T, T >
			towards_infinity(T value)
		{
			return (value >= 0) ? ceil(value) : floor(value);
		}

		template< bool NoMask = false, typename T >
		constexpr enable_if_integral_t< T, T >
			nearest_up(T value, int digits)
		{
			return ((value & bitmask<T>(digits)) >= (1 << (digits - 1))) ? ceil<NoMask>(value, digits) : floor<NoMask>(value, digits);
		}

		template< typename T >
		constexpr enable_if_floating_point_t< T, T >
			nearest_up(T value)
		{
			return (value - floor(value) >= 0.5) ? ceil(value) : floor(value);
		}

		template< bool NoMask = false, typename T >
		constexpr enable_if_integral_t< T, T >
			nearest_down(T value, int digits)
		{
			return ((value & bitmask<T>(digits)) <= (T(1) << (digits - 1))) ? floor<NoMask>(value, digits) : ceil<NoMask>(value, digits);
		}

		template< typename T >
		constexpr enable_if_floating_point_t< T, T >
			nearest_down(T value)
		{
			return (value - floor(value) <= 0.5) ? floor(value) : ceil(value);
		}

		namespace detail {
			// Could separate into Pre and Post round operations to handle floats and integrals equally,
			// but that would just mess up the code with no gains.

			// Float rounding happening in fixed<>::from only

			// Integral rounding happening throughout
			template< RoundModes Mode, bool NoMask = false >
			struct integral_round_switch;

			// Truncate ... fastest rounding possible
			template< bool NoMask >
			struct integral_round_switch< RoundModes::Floor, NoMask >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					// For now I'm relying that the compiler will remove this
					// if a shifting operation follows right after.
					return floor<NoMask>(value, digits);
				}
			};

			template< bool NoMask >
			struct integral_round_switch< RoundModes::Ceil, NoMask >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return ceil<NoMask>(value, digits);
				}
			};

			template< bool NoMask >
			struct integral_round_switch< RoundModes::Zero, NoMask >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return towards_zero<NoMask>(value, digits);
				}
			};

			template< bool NoMask >
			struct integral_round_switch< RoundModes::Infinity, NoMask >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return towards_infinity<NoMask>(value, digits);
				}
			};

			template< bool NoMask >
			struct integral_round_switch< RoundModes::NearestUp, NoMask >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return nearest_up<NoMask>(value, digits);
				}
			};

			template< bool NoMask >
			struct integral_round_switch< RoundModes::NearestDown, NoMask >
			{
				template< typename T >
				static constexpr T round(T value, int digits)
				{
					return nearest_down<NoMask>(value, digits);
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
		constexpr enable_if_floating_point_t<T, T>
			scaled_exp2(T value, int exponent)
		{
			return (exponent >= 0) ? 
				(detail::floating_round_switch<Mode>::round(value * exp2<util::largest_unsigned_type>(exponent))) : (value / exp2<util::largest_unsigned_type>(-exponent));
		}

		template<RoundModes Mode = RoundModes::Floor, typename T>
		constexpr enable_if_integral_t<T, T>
			scaled_exp2(T value, int exponent)
		{
			return (exponent >= 0) ?
				(value << exponent)
				:
				(detail::integral_round_switch<Mode, true>::round(value, -exponent) >> (-exponent));
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

		template<typename T>
		constexpr bool is_neg(T val) {
			return !std::is_unsigned<T>::value && (val < 0);
		}

		template<typename T1, typename T2>
		constexpr bool any_neg(T1 val1, T2 val2)
		{
			return is_neg(val1) || is_neg(val2);
		}

		//
		// Supporting Macros
		//

		// If anybody finds a working alternative that is simpler... please forward to me
		namespace detail {

			template< typename T >
			constexpr int log2_ceil_incr(T value)
			{
				return (value == std::numeric_limits<T>::max()) ? (sizeof(T) * 8) : log2_ceil(value+1);
			}

		}

		template<typename T>
		constexpr enable_if_integral_t<T, int>
			integer_bits(T value)
		{
			return 
				(value == 0) ? 1 : (
					(value > T(0)) ? 
						detail::log2_ceil_incr(abs2(value)) : (log2_ceil(abs(value))+1)
				);
		}

		template<typename T>
		constexpr enable_if_floating_point_t<T, int>
			integer_bits(T value)
		{
			// This one proves to be notorically difficult...
			// (oo ...   1) -> log2_ceil(value+1)
			// ( 2 ...   1) -> 1
			// ( 1 ...   0) -> log2_floor(value)
			// (         0) -> 1
			// ( 0 ...  -1) -> log2_floor(-value)+2
			// [-1 ...  -2) -> 1
			// [-2 ... -oo) -> lof2_floor(-value-1)+2
			return
				(value == 0) ? 1 : (
					(value >= T(2)) ? (log2_ceil(value)+1) : (
						(value >= T(1)) ? (1) : (
							(value >= T(0)) ? (log2_floor(value)+1) : (
								(value > T(-1)) ? (log2_floor(-value)+2) : (
									(value > T(-2)) ? (1) : (log2_floor(-value-T(1))+2)
								)		
							)
						)
					)
				);
		}

		namespace detail {

			template< typename T >
			constexpr T incr_if_neg(T val)
			{
				return is_neg(val) ? (val + T(1)) : val;
			}

			template< typename T, typename U >
			constexpr int range_bits2(T t, U u) {
				// Note, special cases to undo the integer_bits always at least return 1.
				return
					max(
						integer_bits(abs2(t)) - (is_neg(t) ? 1 : 0),
						integer_bits(abs2(u)) - (is_neg(u) ? 1 : 0)


						//(incr_if_neg(t) == 0) ? (0) : integer_bits(abs2(incr_if_neg(t))),
						//(incr_if_neg(u) == 0) ? (0) : integer_bits(abs2(incr_if_neg(u)))
					) 
					+ (any_neg(t,u) ? 1 : 0);
			}

		}

		template<typename S, typename U>
		constexpr int range_bits(S s, U u)
		{
			return (s == 0) ? integer_bits(u) :
				((u == 0) ? integer_bits(s) : detail::range_bits2(s, u));
		}

	}

}

#endif
