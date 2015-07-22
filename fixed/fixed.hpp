//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef UTIL_FIXED_HPP__
#define UTIL_FIXED_HPP__

#include <type_traits>
#include <stdexcept>
#include <fixed/fixedmath.hpp>
#include <fixed/fixedmeta.hpp>

// mst 2015/07/22
// Todo: - improve rounding on scaling shifts with offset bits
//       - add next_even/next_odd rounding mode
//       - see if the shift operations should rather be constexpr free functions
//       - see if fixed::to should rather be a constexpr free function
//       - see if we can scrap offset alltogether since it doesn't bring anything to the table
//       - no rounding applied in scaling shift yet (should really be free function I guess)
//       - hope people find bugs/fixes ;)

namespace fix {

	using int64 = long long;
	using uint64 = unsigned long long;

	//
	// Value type deduction
	//

	namespace detail {

		template< int Bits, bool Signed >
		struct value_type {

			using raw_type = typename std::conditional< (Bits <= 8), char,
				typename std::conditional<(Bits <= 16), short,
				typename std::conditional< (Bits <= 32), int, int64 >::type>::type >::type;

			using type = typename std::conditional<Signed, typename std::make_signed<raw_type>::type, typename std::make_unsigned<raw_type>::type>::type;

		};

		template< int Bits, bool Signed >
		using value_type_t = typename value_type<Bits, Signed>::type;

		template< int I, int F, bool S, int O >
		using fixed_value_type_t = value_type_t<I+F+O, S>;

	}

	//
	// Shifting support
	//

	template< int IntegerBits, int FractionalBits, bool Signed = true, int Offset = 0, typename ValueType = detail::fixed_value_type_t< IntegerBits, FractionalBits, Signed, Offset >>
	class fixed;

	namespace detail {

		// Handles automatic type promotion while shifting a fixed numer up (<<)
		template< typename OriginalType, int Shift >
		struct scaling_shift_values;

		template< int I, int F, bool S, int O, typename ValueType, int Shift >
		struct scaling_shift_values< fixed<I, F, S, O, ValueType>, Shift >
		{
			using original_type = fixed<I, F, S, O, ValueType>;
			using destination_value_type = fixed_value_type_t< I, F+util::min(O+Shift,0), S, util::max(O + Shift, 0) >;

			// We can potentially be much smarter here...
			using precast_type = typename std::conditional< (sizeof(destination_value_type)>sizeof(ValueType)) && (Shift > 0), destination_value_type, ValueType >::type;
			using postcast_type = destination_value_type;

			using result_type = fixed<
				I - util::max<int>(Shift - original_type::free_bits, 0),
				F + util::min<int>(Shift + int(O), 0),
				S,
				util::max<int>(Shift + int(O), 0),
				destination_value_type
			>;
		};

	}

	//
	// Rounding
	//

	using util::RoundModes;

	namespace rounding {

		using ceil = std::integral_constant<RoundModes, RoundModes::Ceil >;
		using floor = std::integral_constant<RoundModes, RoundModes::Floor>;
		using zero = std::integral_constant<RoundModes, RoundModes::Zero>;
		using infinity = std::integral_constant<RoundModes, RoundModes::Infinity>;
		using nearest_odd = std::integral_constant<RoundModes, RoundModes::NearestOdd>;
		using nearest_even = std::integral_constant<RoundModes, RoundModes::NearestEven>;
		using nearest_up = std::integral_constant<RoundModes, RoundModes::NearestUp >;
		using nearest_down = std::integral_constant<RoundModes, RoundModes::NearestDown >;

	}

	//
	// fixed type
	//

	template<int IntegerBits, int FractionalBits, bool Signed, int Offset, typename ValueType>
	class fixed {
	public:
		// The offset might be a waste of effort. It was supposed to set apart irrelevant bits
		// at the LSBs after an up-shift from significant bits.
				
		using value_type  = ValueType;
		
		// If we set up the template argument as unsigned, we would have to cast everywhere
		// becaue C promotes to unsigned...
		static_assert(Offset >= 0, "Offset must be equal or larger than 0");

		// some handy constants
		
		static constexpr int value_bits = static_cast<int>(sizeof(value_type) * 8);

		// number of integer bits
		static constexpr int integer_bits = IntegerBits;
		
		// number of fractional bits
		static constexpr int fractional_bits = FractionalBits;
		
		// number of offset bits at the least significant bits
		static constexpr int offset = Offset;

		// position of radix point. note that this can very well be negative.
		static constexpr int radix_pos = fractional_bits + offset;

		// number of significant bits
		static constexpr int data_bits = integer_bits + fractional_bits;

		// numer of bits used for integer, fractional and offset
		static constexpr int low_bits = data_bits + offset;

		// number of unused bits at the most significant digits
		static constexpr int free_bits = value_bits - low_bits;
		
		// scaling (exp2) which needs to be applied to value to shift the fractional digit before the LSB (ie. to extract the integer-part), !not! considering offset
		static constexpr int scaling = -fractional_bits + util::min(integer_bits, 0);

		// the maximum positive scaling shift which can be applied without promotion
		static constexpr int max_shift = (Signed) ? util::min(free_bits - 1, 0) : free_bits;

		value_type value;

	public:
		fixed()
		{}

		constexpr fixed(value_type value_)
			: value(value_)
		{}

		/*
		constexpr fixed(const fixed& other)
			: value(other.value)
		{}
		*/

	public:
		static constexpr int exponent(int bit)
		{
			return -fractional_bits + util::min(integer_bits, 0) - offset + bit;
		}

		static constexpr int max_exponent()
		{
			return exponent(offset + integer_bits + fractional_bits - 1);
		}

		static constexpr int min_exponent()
		{
			return exponent(offset);
		}

		template< int Shift >
		constexpr auto virtual_shift() const {
			return fixed<integer_bits + Shift, fractional_bits - Shift, offset>(value);
		}

		template< int Shift >
		using scaling_values = detail::scaling_shift_values< fixed, Shift >;

		template< int Shift >
		constexpr typename scaling_values<Shift>::result_type scaling_shift() const {
			// Fixme: no rounding applied here (if shifting down, do everything up to offset without rounding and round the rest accordingly before shifting)
			return typename scaling_values<Shift>::result_type(
				static_cast<typename scaling_values<Shift>::postcast_type>(
					util::shifted( static_cast<typename scaling_values<Shift>::precast_type>(value), Shift)
				)
			);
		}

		template< int Shift >
		using scaling_shifted_type = typename scaling_values<Shift>::result_type;

		constexpr void literal_shift(int Shift) const
		{
		}
		
		template< typename RoundingWrapper = rounding::floor, typename S >
		constexpr static fixed from(S value)
		{
			// Need to be more sophisticated here... detect powers of 2 and such which make negative integers or fractions possible (for constants)
			return
				//( util::test_overflow<value_type, data_bits>(util::round(util::scaled_exp2(value, -scaling))) ) ?
				fixed(static_cast<value_type>(util::scaled_exp2<S, RoundingWrapper::value>(value, -scaling)) << offset) /*: (throw std::logic_error("Out of Bounds."))*/;
		}

		template<typename S, typename RoundingWrapper = rounding::floor>
		constexpr typename std::enable_if<std::is_floating_point<S>::value, S>::type
			to() const
		{
			return util::scaled_exp2<S, RoundingWrapper::value>(value >> offset, scaling);
		}

		template<typename S, typename RoundingWrapper = rounding::floor>
		constexpr typename std::enable_if<std::is_integral<S>::value, S>::type
			to() const
		{
			return util::scaled_exp2<S, RoundingWrapper::value>(value >> offset, scaling);
		}

	};

	template< int I, int F >
	using ufixed = fixed<I, F, false>;

	template< int I, int F >
	using sfixed = fixed<I, F, true>;

	template< int I, int F = std::numeric_limits<int>::max() >
	struct fits {
		static constexpr int integer_bits = I;
		static constexpr int fraction_bits = F;
	};

	//
	// Various operation arguments
	//

	template< int Value >
	struct max_size {
		static constexpr int value = Value;
	};

	using platform_max_size = max_size<32>;
	
	struct positive {};

	namespace detail {

		struct fits_finder {
			template< typename T >
			struct apply : public meta::false_type
			{};

			template< int I, int F >
			struct apply< fits< I, F > > : public meta::true_type
			{
				using type = fits<I, F>;
			};
		};

		struct max_size_finder {
			template< typename T >
			struct apply : public meta::false_type
			{};

			template< int Value >
			struct apply< max_size<Value> > : public meta::true_type
			{
				using type = max_size<Value>;
			};
		};

	}


	//
	// Division
	//

	namespace detail {

		template< typename ArgList >
		struct div_args
		{
			// Assume positive result, no matter if arguments are signed
			static constexpr bool assume_result_positive = meta::contains< ArgList, positive >::value;
			
			// Check for contrained result range
			using result_range = typename meta::find_if_or< ArgList, fits_finder, fits<std::numeric_limits<int>::max()> >::type;
			static constexpr bool constrained_integer = (result_range::integer_bits != std::numeric_limits<int>::max());
			static constexpr bool constrained_fraction = (result_range::fraction_bits != std::numeric_limits<int>::max());

			// Check for rounding mode
			static constexpr RoundModes rounding = meta::find_if_or< ArgList, meta::integral_constant_finder<RoundModes>, rounding::floor >::type::value;

			// Check for maximum bitcount in intermediates
			static constexpr int max_size = meta::find_if_or< ArgList, max_size_finder, platform_max_size>::type::value;

			static_assert(max_size <= platform_max_size::value, "You are exceeding your platform's maximum bitcount.");
		};

		template< typename A, typename B >
		struct auto_type {
			using intermediate = typename std::conditional< (sizeof(A) >= sizeof(B)), A, B > ::type;
			using type = 
				typename std::conditional< std::is_signed<A>::value || std::is_signed<B>::value, typename std::make_signed<intermediate>::type, typename std::make_unsigned<intermediate>::type >::type;
		};

		template< typename Args, typename Nom, typename Denom >
		struct div_struct;

		template< typename Args, typename NomT, int NomI, int NomF, bool NomS, int NomO, typename DenT, int DenI, int DenF, bool DenS, int DenO >
		struct div_struct< Args, fixed<NomI,NomF,NomS,NomO, NomT>, fixed<DenI, DenF, DenS, DenO, DenT>>  {
			using nom_type = fixed<NomI, NomF, NomS, NomO, NomT>;
			using den_type = fixed<DenI, DenF, DenS, DenO, DenT>;
			using parsed_args = div_args<Args>;

			using result_value_type = typename auto_type<NomT, DenT>::type;

			static constexpr int auto_i = NomI + DenF;
			static constexpr bool source_signed = NomS || DenS;
			static constexpr int result_i = parsed_args::constrained_integer ? parsed_args::result_range::integer_bits : auto_i;
			static constexpr int auto_f = int(sizeof(result_value_type) * 8) - result_i;
			static constexpr int result_f = parsed_args::constrained_fraction ? parsed_args::result_range::fraction_bits : auto_f;

			static constexpr int max_size = parsed_args::max_size;

			static constexpr bool assume_positive = parsed_args::assume_result_positive;

			using result_type = fixed<result_i, result_f, source_signed && !assume_positive>;

			// should not assert ever, at least if the user doesn't give I and F and a max_size which doesn't fit them.
			static_assert(result_i + result_f <= max_size, "Cannot divide with given constraints.");

			// exp(result.n) = exp(nom.0) - exp(den.0) + n
			static constexpr int shift_nom_exact = nom_type::exponent(0) + den_type::fractional_bits + result_f;
			static constexpr int shift_nom = util::min<int>(shift_nom_exact, max_size - nom_type::offset - nom_type::fractional_bits - nom_type::integer_bits);
			static constexpr int shift_den = -den_type::offset - util::max<int>(shift_nom_exact-shift_nom, 0);
			
			static constexpr result_type divide(nom_type nom, den_type den)
			{
				return result_type(nom.template scaling_shift<shift_nom>().value / den.template scaling_shift<shift_den>().value);
			}
		};
	}

	template< typename... Args, typename NomT, int NomI, int NomF, bool NomS, int NomO, typename DenT, int DenI, int DenF, bool DenS, int DenO>
	constexpr auto div(fixed<NomI, NomF, NomS, NomO, NomT> nom, fixed<DenI, DenF, DenS, DenO, DenT> den)
	{
		return detail::div_struct<meta::list<Args...>, fixed<NomI, NomF, NomS, NomO, NomT>, fixed<DenI, DenF, DenS, DenO, DenT>>::divide(nom, den);
	}

	//
	// Multiplication
	//

	namespace detail {

		template< typename ArgList >
		struct mul_args
		{
			// Assume positive result, no matter if arguments are signed
			static constexpr bool assume_result_positive = meta::contains< ArgList, positive >::value;

			// Check for contrained result range
			using result_range = typename meta::find_if_or< ArgList, fits_finder, fits<std::numeric_limits<int>::max()> >::type;
			static constexpr bool constrained_integer = (result_range::integer_bits != std::numeric_limits<int>::max());
			static constexpr bool constrained_fraction = (result_range::fraction_bits != std::numeric_limits<int>::max());

			// Check for rounding mode
			static constexpr RoundModes rounding = meta::find_if_or< ArgList, meta::integral_constant_finder<RoundModes>, rounding::floor >::type::value;

			// Check for maximum bitcount in intermediates
			static constexpr int max_size = meta::find_if_or< ArgList, max_size_finder, platform_max_size>::type::value;

			static_assert(max_size <= platform_max_size::value, "You are exceeding your platform's maximum bitcount.");
		};

		template< typename Args, typename Nom, typename Denom >
		struct mul_struct;

		template< typename Args, typename AT, int AI, int AF, bool AS, int AO, typename BT, int BI, int BF, bool BS, int BO >
		struct mul_struct< Args, fixed<AI, AF, AS, AO, AT>, fixed<BI, BF, BS, BO, BT>> {
			using a_type = fixed<AI, AF, AS, AO, AT>;
			using b_type = fixed<BI, BF, BS, BO, BT>;
			using parsed_args = mul_args<Args>;
			
			// the result range possibly specified by fits<A(,B)>
			using result_range = typename parsed_args::result_range;

			// the automatic result value type, if not otherwise constrained by user
			using result_value_type = typename auto_type<AT, BT>::type;

			static constexpr int  auto_i = AI + BI;
			static constexpr bool auto_s = AS || BS;

			static constexpr int result_i = parsed_args::constrained_integer ? result_range::integer_bits : auto_i;
			static constexpr int auto_f   = int(sizeof(result_value_type) * 8) - result_i;
			static constexpr int result_f = parsed_args::constrained_fraction ? result_range::fraction_bits : auto_f;

			static constexpr int max_size = parsed_args::max_size;
			static constexpr bool assume_positive = parsed_args::assume_result_positive;

			// Check to see if temporary overshoots the maximum given bitcount
			static constexpr int overshoot = util::max(AI + AF + AO + BI + BF + BO - max_size, 0);
			
			static constexpr int a_shift_temp = -util::min(AO, overshoot);
			static constexpr int b_shift_temp = -util::min(BO, overshoot + a_shift_temp);
			
			static constexpr int remaining_overshoot = util::max(overshoot + a_shift_temp + b_shift_temp, 0);
			static constexpr int a_shift = a_shift_temp - remaining_overshoot / 2;
			static constexpr int b_shift = b_shift_temp - remaining_overshoot / 2 - remaining_overshoot % 2;
			
			using shifted_a_type = typename a_type::template scaling_shifted_type<a_shift>;
			using shifted_b_type = typename b_type::template scaling_shifted_type<b_shift>;

			static_assert((shifted_a_type::low_bits + shifted_b_type::low_bits) <= max_size, "Overshooting temporary.");

			using mul_result_value_type = value_type_t<shifted_a_type::low_bits + shifted_b_type::low_bits, auto_s>;
			static constexpr int mul_result_radix_pos = shifted_a_type::radix_pos + shifted_b_type::radix_pos;
			static constexpr int result_radix_pos = result_f;

			static constexpr int result_shift = result_radix_pos - mul_result_radix_pos;

			using result_type = fixed<result_i, result_f, auto_s && !assume_positive>;

			// should not assert ever, at least if the user doesn't give I and F and a max_size which doesn't fit them.
			static_assert(result_i + result_f <= max_size, "Cannot multiply with given constraints.");

			static constexpr result_type mul(a_type a, b_type b)
			{
				return result_type( static_cast<typename result_type::value_type>(
					util::scaled_exp2<mul_result_value_type, parsed_args::rounding>(
						mul_result_value_type(a.template scaling_shift<a_shift>().value) * b.template scaling_shift<b_shift>().value,
						result_shift)
					) );
			}
		};
	}

	template< typename... Args, typename AT, int AI, int AF, bool AS, int AO, typename BT, int BI, int BF, bool BS, int BO>
	constexpr auto mul(fixed<AI, AF, AS, AO, AT> a, fixed<BI, BF, BS, BO, BT> b)
	{
		return detail::mul_struct<meta::list<Args...>, fixed<AI, AF, AS, AO, AT>, fixed<BI, BF, BS, BO, BT>>::mul(a, b);
	}


	//
	// Sum / Difference
	//

	namespace detail {

		template< typename ArgList >
		struct add_sub_args
		{
			// Assume positive result, no matter if arguments are signed
			static constexpr bool assume_result_positive = meta::contains< ArgList, positive >::value;

			// Check for contrained result range
			using result_range = typename meta::find_if_or< ArgList, fits_finder, fits<std::numeric_limits<int>::max()> >::type;
			static constexpr bool constrained_integer = (result_range::integer_bits != std::numeric_limits<int>::max());
			static constexpr bool constrained_fraction = (result_range::fraction_bits != std::numeric_limits<int>::max());

			// Check for rounding mode
			static constexpr RoundModes rounding = meta::find_if_or< ArgList, meta::integral_constant_finder<RoundModes>, rounding::floor >::type::value;

			// Check for maximum bitcount in intermediates
			static constexpr int max_size = meta::find_if_or< ArgList, max_size_finder, platform_max_size>::type::value;

			static_assert(max_size <= platform_max_size::value, "You are exceeding the platform's maximum bitcount.");
		};

		template< typename ArgList, typename A, typename B >
		struct add_sub_struct;

		// Find the best combination of shifts so the fractional points are aligned,
		// minimizing shift operations but only if no precision is lost
		template< typename ArgList, int AI, int AF, bool AS, int AO, typename AT, int BI, int BF, bool BS, int BO, typename BT >
		struct add_sub_struct< ArgList, fixed<AI, AF, AS, AO, AT>, fixed<BI, BF, BS, BO, BT> >
		{
			// This is a mess!
			using A = fixed<AI, AF, AS, AO, AT>;
			using B = fixed<BI, BF, BS, BO, BT>;

			// Collect template arguments
			using parsed_args = add_sub_args< ArgList >;

			static constexpr int max_size = parsed_args::max_size;
			using result_range = typename parsed_args::result_range;

			constexpr static bool i_constrained = parsed_args::constrained_integer;
			constexpr static bool f_constrained = parsed_args::constrained_fraction;

			// Determine lhs and rhs scaling shifts to match radix points (offset difference will be handled in next step)
			constexpr static int exponent_difference = B::fractional_bits - A::fractional_bits;

			constexpr static int a_shift_preliminary = util::max( exponent_difference, 0) + util::min(B::offset - A::offset, 0);
			constexpr static int b_shift_preliminary = util::max(-exponent_difference, 0) + util::min(A::offset - B::offset, 0);

			// If either side is overshooting the maximum bit size, shift both down accordingly
			constexpr static int overshoot  = util::max(util::max((A::low_bits + a_shift_preliminary) - max_size, (B::low_bits + b_shift_preliminary) - max_size), 0);

			// If custom-dest fraction bits lead to a negative offset, shift both up accordingly 
			constexpr static int undershoot = f_constrained ? (-util::min(A::radix_pos + a_shift_preliminary - overshoot - result_range::fraction_bits, 0)) : (0);

			constexpr static int a_shift = a_shift_preliminary - overshoot + undershoot;
			constexpr static int b_shift = b_shift_preliminary - overshoot + undershoot;

			constexpr static bool RS_sum = parsed_args::assume_result_positive ? false : (AS || BS);
			// if not otherwise expressed by an argument, always assume signed destination range for subtraction
			constexpr static bool RS_sub = parsed_args::assume_result_positive ? false : true;

			// need to find out result-type
			using shifted_a_type = typename A::template scaling_shifted_type<a_shift>;
			using shifted_b_type = typename B::template scaling_shifted_type<b_shift>;

			static_assert(shifted_a_type::exponent(0) == shifted_b_type::exponent(0), "My calculations were wrong");
			static_assert(shifted_a_type::low_bits <= max_size && shifted_b_type::low_bits <= max_size, "Could not fit the calculation.");

			static constexpr int result_i = i_constrained ?  result_range::integer_bits  : util::max(shifted_a_type::integer_bits, shifted_b_type::integer_bits   );
			static constexpr int result_f = f_constrained ?  result_range::fraction_bits : util::max(shifted_a_type::fractional_bits, shifted_b_type::fractional_bits);
			static constexpr int result_o = f_constrained ? (shifted_a_type::radix_pos-result_f) : util::min(shifted_a_type::offset, shifted_b_type::offset);

			using sub_result_type = fixed<result_i, result_f, RS_sub, result_o>;
			using add_result_type = fixed<result_i, result_f, RS_sum, result_o>;

			constexpr static add_result_type add(A a, B b) {
				return add_result_type(a.template scaling_shift<a_shift>().value + b.template scaling_shift<b_shift>().value);
			}

			constexpr static sub_result_type sub(A a, B b) {
				return sub_result_type(a.template scaling_shift<a_shift>().value - b.template scaling_shift<b_shift>().value);
			}
		};
	}

	template< typename... Args, typename AT, int AI, int AF, bool AS, int AO, typename BT, int BI, int BF, bool BS, int BO >
	constexpr auto add(fixed<AI, AF, AS, AO, AT> a, fixed<BI, BF, BS, BO, BT> b)
	{
		return detail::add_sub_struct< meta::list<Args...>, decltype(a), decltype(b) >::add(a, b);
	}

	template< typename... Args, typename AT, int AI, int AF, bool AS, int AO, typename BT, int BI, int BF, bool BS, int BO >
	constexpr auto sub(fixed<AI, AF, AS, AO, AT> a, fixed<BI, BF, BS, BO, BT> b)
	{
		return detail::add_sub_struct< meta::list<Args...>, fixed<AI, AF, AS, AO, AT>, fixed<BI, BF, BS, BO, BT> >::sub(a, b);
	}

	template< typename T >
	constexpr fixed<sizeof(T) * 8, 0, std::is_signed<T>::value> integer(T value)
	{
		return fixed<sizeof(T) * 8, 0, std::is_signed<T>::value>(value);
	}

}

#define FIXED_TYPE_S(Value, Size) \
	::fix::fixed<::fix::util::integer_bits(Value), Size-::fix::util::integer_bits(Value), (Value<0)>

#define FIXED_TYPE_P(Value, Precision) \
	::fix::fixed<::fix::util::integer_bits(Value), Precision, (Value<0)>

#define FIXED_VALUE_S(Value, Size) \
	FIXED_TYPE_S(Value, Size)::from(Value)

#define FIXED_VALUE_P(Value, Precision) \
	FIXED_TYPE_P(Value, Precision)::from(Value)

#define FIXED_RANGE_TYPE_S(Min, Max, Size) \
	::fix::fixed<::fix::util::integer_bits_interval(Min,Max), Size - ::fix::util::integer_bits_interval(Min,Max), (Min < 0 || Max < 0)>

#define FIXED_RANGE_TYPE_P(Min, Max, Precision) \
	::fix::fixed<::fix::util::integer_bits_interval(Min,Max), Precision, (Min < 0 || Max < 0)>

#endif
