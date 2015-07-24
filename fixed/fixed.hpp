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

#define SC static constexpr

	using int64 = long long;
	using uint64 = unsigned long long;

	//
	// Value type deduction
	//
	
	namespace detail {
	
		template< typename T, bool Signed = std::is_signed<T>::value >
		struct promoted
		{
			using bare_type = typename std::conditional< sizeof(T) == 1, short,
				typename std::conditional< sizeof(T) == 2, int,
				typename std::conditional< sizeof(T) == 4, int64, int64>::type
				>::type >::type;

			using type = typename std::conditional< Signed, typename std::make_signed<bare_type>::type, typename std::make_unsigned<bare_type>::type>::type;
		};

		template< typename T, bool Signed = std::is_signed<T>::value >
		using promoted_t = typename promoted<T, Signed>::type;

		template< typename T, T value >
		constexpr auto promote_if_max()
		{
			return typename std::conditional<value == std::numeric_limits<T>::max(), promoted_t<T>, T>::type(value);
		}



		template< typename T1, typename T2 >
		struct fitting_type {
			using type = typename std::conditional<  
				sizeof(T1) == sizeof(T2) && !(std::is_signed<T1>::value == std::is_signed<T2>::value),
				promoted_t<T1, true>,
				typename std::conditional<(sizeof(T1)>sizeof(T2)), T1, T2>::type
			>::type;
		};

		template< typename T1, typename T2 >
		using fitting_type_t = typename fitting_type<T1, T2>::type;

		template< int Bits, bool Signed >
		struct value_type {

			using raw_type = typename std::conditional< (Bits <= 8), char,
				typename std::conditional<(Bits <= 16), short,
				typename std::conditional< (Bits <= 32), int, int64 >::type>::type >::type;

			using type = typename std::conditional<Signed, typename std::make_signed<raw_type>::type, typename std::make_unsigned<raw_type>::type>::type;

		};

		template< int Bits, bool Signed >
		using value_type_t = typename value_type<Bits, Signed>::type;

		template< int I, int F, bool Signed, typename T>
		constexpr auto to_fixed(T value)
		{
			return static_cast<value_type_t<I + F, Signed>>(util::scaled_exp2<RoundModes::Floor>(value, F));
		}
	}

	template< typename T, T Min, T Max >
	struct value_range {
		using value_type = T;
		SC T min = Min;
		SC T max = Max;
		SC bool is_signed = (min < 0) || (max < 0);
		SC int bits = util::bits_for_range<T, min, max>::value;
		using min_type       = detail::value_type_t<bits, is_signed>;
		using min_range_type = value_range<min_type, Min, Max>;
	};
	
#define RANGE_FROM_VALS(A,B) \
	value_range<detail::fitting_type_t<decltype(A), decltype(B)>, A, B>::min_range_type

	namespace detail {

		template< int I, int F, bool S, int O >
		using fixed_value_type_t = value_type_t<I+F+O, S>;
		
		template< int Bits, bool Signed, typename ValueType = value_type_t<Bits, Signed>>
		constexpr ValueType max_value()
		{
			return Signed ? util::bitmask<ValueType>(Bits-1) : util::bitmask<ValueType>(Bits);
		}

		template< int Bits, bool Signed, typename ValueType = value_type_t<Bits, Signed> >
		constexpr ValueType min_value()
		{
			return Signed ? (~util::bitmask<ValueType>(Bits-1)) : ValueType(0);
		}

		template< int Bits, bool Signed, typename ValueType = value_type_t<Bits, Signed> >
		using auto_fixed_range = value_range<ValueType, min_value<Bits, Signed, ValueType>(), max_value<Bits, Signed, ValueType >()>;

		template< int Bits, bool Signed, typename ValueType = value_type_t<Bits, Signed>>
		struct minmax {
			static constexpr ValueType min = min_value<util::min(Bits,1),Signed, ValueType>();
			static constexpr ValueType max = max_value<util::min(Bits,1),Signed, ValueType>();
		};

	}

	//
	// Shifting support
	//

	template < int I, int F, bool S = true, int O = 0, typename ValueType = detail::value_type_t<I + F + O, S>, typename Range = detail::auto_fixed_range<I+F+O, S> >
	class fixed;

	namespace detail {

		// Handles automatic type promotion while shifting a fixed numer up (<<)
		template< typename OriginalType, int Shift >
		struct scaling_shift_values;

		template< int I, int F, bool S, int O, typename ValueType, typename Range, int Shift >
		struct scaling_shift_values< fixed<I, F, S, O, ValueType, Range>, Shift >
		{
			using original_type = fixed<I, F, S, O, ValueType, Range>;
			using destination_value_type = fixed_value_type_t< I, F+util::min(O+Shift,0), S, util::max(O + Shift, 0) >;

			// We can potentially be much smarter here...
			using precast_type = typename std::conditional< (sizeof(destination_value_type)>sizeof(ValueType)) && (Shift > 0), destination_value_type, ValueType >::type;
			using postcast_type = destination_value_type;

			static constexpr int precast_bits = sizeof(precast_type) * 8;
			static constexpr int free_bits = precast_bits - original_type::data_bits;

			using result_type = fixed<
				I - util::max<int>(Shift - free_bits, 0),
				F + util::min<int>(Shift + int(O), 0),
				S,
				util::max<int>(Shift + int(O), 0),
				destination_value_type,
				value_range<destination_value_type, util::shifted(destination_value_type(Range::min), Shift), util::shifted(destination_value_type(Range::max), Shift)>
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

	template<int I, int F, bool S, int O, typename ValueType, typename RangeType>
	class fixed {
	public:
		// The offset might be a waste of effort. It was supposed to set apart irrelevant bits
		// at the LSBs after an up-shift from significant bits.
				
		using value_type  = ValueType;
		using range_type = RangeType;
		
		// If we set up the template argument as unsigned, we would have to cast everywhere
		// becaue C promotes to unsigned...
		static_assert(O >= 0, "Offset must be equal or larger than 0");

		// some handy constants
		SC bool is_signed = S;

		SC int value_bits = static_cast<int>(sizeof(value_type) * 8);

		// number of integer bits
		SC int integer_bits = I;
		
		// if we're having a signed X.0 type, then >> unsigned bits only leaves the sign in front of the radix point
		SC int unsigned_bits = (is_signed) ? (integer_bits - 1) : integer_bits;

		// number of fractional bits
		SC int fractional_bits = F;
		
		// number of offset bits at the least significant bits
		SC int offset = O;

		// position of radix point. note that this can very well be negative.
		SC int radix_pos = fractional_bits + offset;

		// number of significant bits
		SC int data_bits = integer_bits + fractional_bits;

		// numer of bits used for integer, fractional and offset
		SC int low_bits = data_bits + offset;

		// number of unused bits at the most significant digits
		SC int free_bits = value_bits - low_bits;
		
		// scaling (exp2) which needs to be applied to value to shift the fractional digit before the LSB (ie. to extract the integer-part), !not! considering offset
		SC int scaling = -fractional_bits;

		// the maximum positive scaling shift which can be applied without promotion
		SC int max_shift = (S) ? util::min(free_bits - 1, 0) : free_bits;

		value_type value;

	public:
		fixed()
		{}

		constexpr fixed(value_type value_)
			: value(value_)
		{}


		template<int Off, typename T, typename R>
		fixed& operator=(fixed<I, F, S, Off, T, R> other)
		{
			value = other.scaling_shift<offset - Off>().value;
			return *this;
		}
		
		/*
		constexpr fixed(const fixed& other)
			: value(other.value)
		{}
		*/

	public:
		static constexpr int exponent(int bit)
		{
			return -fractional_bits - offset + bit;
		}

		static constexpr int max_exponent()
		{
			return exponent(offset + integer_bits + fractional_bits - 1);
		}

		static constexpr int min_exponent()
		{
			return exponent(offset);
		}

		template< typename T >
		constexpr auto to_type() const
		{
			return fixed<integer_bits + ((!is_signed && std::is_signed<T>::value) ? 1 : 0), fractional_bits, std::is_signed<T>::value, offset, T, range_type>(value);
		}

		template< int Shift >
		constexpr auto virtual_shift() const {
			return fixed<integer_bits + Shift, fractional_bits - Shift, S, offset, value_type, range_type>(value);
		}

		static constexpr fixed max()
		{
			return fixed(static_cast<value_type>(range_type::max));
		}

		static constexpr fixed min()
		{
			return fixed(static_cast<value_type>(range_type::min));
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
		
		template< typename RoundingWrapper = rounding::floor, typename U >
		constexpr static fixed from(U value)
		{
			// Need to be more sophisticated here... detect powers of 2 and such which make negative integers or fractions possible (for constants)
			return
				//( util::test_overflow<value_type, data_bits>(util::round(util::scaled_exp2(value, -scaling))) ) ?
				fixed(static_cast<value_type>(util::scaled_exp2<RoundingWrapper::value>(value, -scaling)) << offset) /*: (throw std::logic_error("Out of Bounds."))*/;
		}

		template<typename U, typename RoundingWrapper = rounding::floor>
		constexpr typename std::enable_if<std::is_floating_point<U>::value, U>::type
			to() const
		{
			return util::scaled_exp2<RoundingWrapper::value, U>(value >> offset, scaling);
		}

		template<typename U, typename RoundingWrapper = rounding::floor>
		constexpr typename std::enable_if<std::is_integral<U>::value, U>::type
			to() const
		{
			return util::scaled_exp2<RoundingWrapper::value, U>(value >> offset, scaling);
		}

	};

	template< int I, int F, typename RangeType = detail::auto_fixed_range<I + F, true> >
	using ufixed = fixed<I, F, false, 0, detail::value_type_t<I+F, false>, RangeType>;

	template< int I, int F, typename RangeType = detail::auto_fixed_range<I+F, true> >
	using sfixed = fixed<I, F, true, 0, detail::value_type_t<I+F,true>, RangeType>;

	template< int I, int F, bool S, typename R >
	using fixed_auto = ::fix::fixed<I, F, S, 0, ::fix::detail::value_type_t<I + F, S>, R>;

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

	using platform_max_size = max_size<64>;
	
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

		template< typename Args, typename NomT, int NomI, int NomF, bool NomS, int NomO, typename NomR, typename DenT, int DenI, int DenF, bool DenS, int DenO, typename DenR >
		struct div_struct< Args, fixed<NomI,NomF,NomS,NomO, NomT, NomR>, fixed<DenI, DenF, DenS, DenO, DenT, DenR>>  {
			using nom_type = fixed<NomI, NomF, NomS, NomO, NomT, NomR>;
			using den_type = fixed<DenI, DenF, DenS, DenO, DenT, DenR>;
			using parsed_args = div_args<Args>;

			using auto_result_value_type = typename auto_type<NomT, DenT>::type;

			static constexpr int auto_i = NomI + DenF;
			static constexpr bool source_signed = NomS || DenS;
			static constexpr int result_i = parsed_args::constrained_integer ? parsed_args::result_range::integer_bits : auto_i;
			static constexpr int auto_f = int(sizeof(auto_result_value_type) * 8) - result_i;
			static constexpr int result_f = parsed_args::constrained_fraction ? parsed_args::result_range::fraction_bits : auto_f;

			static constexpr int max_size = parsed_args::max_size;

			static constexpr bool assume_positive = parsed_args::assume_result_positive;

			using result_type = fixed<result_i, result_f, source_signed && !assume_positive>;
			using result_value_type = typename result_type::value_type;

			// should not assert ever, at least if the user doesn't give I and F and a max_size which doesn't fit them.
			static_assert(result_i + result_f <= max_size, "Cannot divide with given constraints.");

			// exp(result.n) = exp(nom.0) - exp(den.0) + n
			static constexpr int shift_nom_exact = nom_type::exponent(0) + den_type::fractional_bits + result_f;
			static constexpr int shift_nom = util::min<int>(shift_nom_exact, max_size - nom_type::offset - nom_type::fractional_bits - nom_type::integer_bits);
			static constexpr int shift_den = -den_type::offset - util::max<int>(shift_nom_exact-shift_nom, 0);
			
			static constexpr result_type divide(nom_type nom, den_type den)
			{
				return result_type(
					static_cast<result_value_type>(nom.template scaling_shift<shift_nom>().value / den.template scaling_shift<shift_den>().value )
					);
			}
		};
	}

	template< typename... Args, typename NomT, int NomI, int NomF, bool NomS, int NomO, typename NomR, typename DenT, int DenI, int DenF, bool DenS, int DenO, typename DenR>
	constexpr auto div(fixed<NomI, NomF, NomS, NomO, NomT, NomR> nom, fixed<DenI, DenF, DenS, DenO, DenT, DenR> den)
	{
		return detail::div_struct<meta::list<Args...>, fixed<NomI, NomF, NomS, NomO, NomT, NomR>, fixed<DenI, DenF, DenS, DenO, DenT, DenR>>::divide(nom, den);
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

		template< typename RangeA, typename RangeB >
		using mul_result_type = detail::value_type_t<(sizeof(typename RangeA::value_type) + sizeof(typename RangeB::value_type)) * 8, std::is_signed<typename RangeA::value_type>::value || std::is_signed<typename RangeB::value_type>::value >;

		template< typename RangeA, typename RangeB, typename result_type = mul_result_type<RangeA, RangeB> >
		struct max_mul_result {
			SC auto value = util::max(
				result_type(RangeA::max) * RangeB::max,
				result_type(RangeA::max) * RangeB::min,
				result_type(RangeA::min) * RangeB::max,
				result_type(RangeA::min) * RangeB::min
				);
		};

		template< typename RangeA, typename RangeB, typename result_type = mul_result_type<RangeA,RangeB> >
		struct min_mul_result {
			SC auto value = util::min(
				result_type(RangeA::max) * RangeB::max,
				result_type(RangeA::max) * RangeB::min,
				result_type(RangeA::min) * RangeB::max,
				result_type(RangeA::min) * RangeB::min
				);
		};

		template< typename RangeA, typename RangeB >
		using mul_result_range = value_range< mul_result_type<RangeA, RangeB>, min_mul_result<RangeA, RangeB>::value, max_mul_result<RangeA, RangeB>::value >;

		template< typename Args, typename Nom, typename Denom >
		struct mul_struct;

		template< typename Args, typename AT, int AI, int AF, bool AS, int AO, typename AR, typename BT, int BI, int BF, bool BS, int BO, typename BR >
		struct mul_struct< Args, fixed<AI, AF, AS, AO, AT, AR>, fixed<BI, BF, BS, BO, BT, BR>> {
			using a_type = fixed<AI, AF, AS, AO, AT, AR>;
			using b_type = fixed<BI, BF, BS, BO, BT, BR>;
			using parsed_args = mul_args<Args>;
			
			// the result range possibly specified by fits<A(,B)>
			using result_range = typename parsed_args::result_range;

			// the automatic result value type, if not otherwise constrained by user
			using result_value_type = typename auto_type<AT, BT>::type;
			using range = mul_result_range<AR, BR>;

			static constexpr bool assume_positive = parsed_args::assume_result_positive;

			//static constexpr int  auto_i = AI + BI;
			static constexpr bool stripped_sign = (AS || BS) && assume_positive;
			static constexpr bool auto_s = (AS || BS) && (!assume_positive);

			SC int auto_i = range::bits - AF - BF - AO - BO - (stripped_sign ? 1 : 0);

			static constexpr int result_i = parsed_args::constrained_integer ? result_range::integer_bits : auto_i;
			static constexpr int auto_f   = int(sizeof(result_value_type) * 8) - result_i;
			static constexpr int result_f = parsed_args::constrained_fraction ? result_range::fraction_bits : auto_f;

			static constexpr int max_size = parsed_args::max_size;
	
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

			using range_value_type = typename range::value_type;
			static constexpr auto result_min = (assume_positive && range::min < 0) ? range_value_type(0) : (util::shifted(range::min, result_shift));
			static constexpr auto result_max = util::shifted(range::max, result_shift);

			using result_range_type = typename value_range<range_value_type, result_min, result_max>::min_range_type;

			using result_type = fixed_auto<result_i, result_f, auto_s && !assume_positive, result_range_type>;

			// should not assert ever, at least if the user doesn't give I and F and a max_size which doesn't fit them.
			static_assert(result_i + result_f <= max_size, "Cannot multiply with given constraints.");

			static constexpr result_type mul(a_type a, b_type b)
			{
				return result_type( static_cast<typename result_type::value_type>(
					util::scaled_exp2<parsed_args::rounding, mul_result_value_type>(
						mul_result_value_type(a.template scaling_shift<a_shift>().value) * b.template scaling_shift<b_shift>().value,
						result_shift)
					) );
			}
		};

	}

	template< typename... Args, typename AT, int AI, int AF, bool AS, int AO, typename AR, typename BT, int BI, int BF, bool BS, int BO, typename BR>
	constexpr auto mul(fixed<AI, AF, AS, AO, AT, AR> a, fixed<BI, BF, BS, BO, BT, BR> b)
	{
		return detail::mul_struct<meta::list<Args...>, fixed<AI, AF, AS, AO, AT, AR>, fixed<BI, BF, BS, BO, BT, BR>>::mul(a, b);
	}

#define DEBUG_MUL( A, B, ... ) \
	::fix::detail::mul_struct< ::fix::meta::list< __VA_ARGS__ >,  std::decay_t<decltype(A)>, std::decay_t<decltype(B)> >

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
			static constexpr bool max_size_constrained = !std::is_same< typename meta::find_if<ArgList, max_size_finder>::type, meta::void_type >::value;

			static_assert(max_size <= platform_max_size::value, "You are exceeding the platform's maximum bitcount.");
		};

		template< typename RangeA, typename RangeB >
		using sub_result_type = detail::value_type_t<
			util::min<int>(
				util::max(sizeof(typename RangeA::min_type), sizeof(typename RangeB::min_type))*8+1,
				64
				), true>;

		template< typename RangeA, typename RangeB, typename result_type = sub_result_type<RangeA, RangeB> >
		struct max_sub_result {
			SC auto value = util::max(
				result_type(RangeA::max) - RangeB::max,
				result_type(RangeA::max) - RangeB::min,
				result_type(RangeA::min) - RangeB::max,
				result_type(RangeA::min) - RangeB::min
				);
		};

		template< typename RangeA, typename RangeB, typename result_type = sub_result_type<RangeA, RangeB> >
		struct min_sub_result {
			SC auto value = util::min(
				result_type(RangeA::max) - RangeB::max,
				result_type(RangeA::max) - RangeB::min,
				result_type(RangeA::min) - RangeB::max,
				result_type(RangeA::min) - RangeB::min
				);
		};

		template< typename ArgList, typename A, typename B >
		struct add_sub_struct;

		// Find the best combination of shifts so the fractional points are aligned,
		// minimizing shift operations but only if no precision is lost
		template< typename ArgList, int AI, int AF, bool AS, int AO, typename AT, typename AR, int BI, int BF, bool BS, int BO, typename BT, typename BR >
		struct add_sub_struct< ArgList, fixed<AI, AF, AS, AO, AT, AR>, fixed<BI, BF, BS, BO, BT, BR> >
		{
			// This is a mess!
			using A = fixed<AI, AF, AS, AO, AT, AR>;
			using B = fixed<BI, BF, BS, BO, BT, BR>;

			using parsed_args = add_sub_args< ArgList >;

			static constexpr int max_size = parsed_args::max_size_constrained ? parsed_args::max_size : sizeof(typename auto_type<AT, BT>::type)*8;
			using result_range = typename parsed_args::result_range;

			constexpr static bool i_constrained = parsed_args::constrained_integer;
			constexpr static bool f_constrained = parsed_args::constrained_fraction;

			// Determine lhs and rhs scaling shifts to match radix points (offset difference will be handled in next step)
			constexpr static int exponent_difference = B::fractional_bits - A::fractional_bits;

			constexpr static int a_shift_preliminary = util::max( exponent_difference, 0) + util::min(B::offset - A::offset, 0);
			constexpr static int b_shift_preliminary = util::max(-exponent_difference, 0) + util::min(A::offset - B::offset, 0);

			constexpr static bool RS_sum = parsed_args::assume_result_positive ? false : (AS || BS);
			// if not otherwise expressed by an argument, always assume signed destination range for subtraction
			constexpr static bool RS_sub = parsed_args::assume_result_positive ? false : true;

			//
			constexpr static int a_sign_extension = (!AS && BS) ? 1 : 0;
			constexpr static int b_sign_extension = (!BS && AS) ? 1 : 0;

			using a_extended_type = typename std::conditional<(!AS && BS), typename std::make_signed<typename A::value_type>::type, typename A::value_type>::type;
			using b_extended_type = typename std::conditional<(!BS && AS), typename std::make_signed<typename B::value_type>::type, typename B::value_type>::type;
			
			// If either side is overshooting the maximum bit size, shift both down accordingly
			constexpr static int overshoot  = util::max(util::max((A::low_bits + a_shift_preliminary + a_sign_extension) - max_size, (B::low_bits + b_shift_preliminary + b_sign_extension) - max_size), 0);

			// If custom-dest fraction bits lead to a negative offset, shift both up accordingly 
			constexpr static int undershoot = f_constrained ? (-util::min(A::radix_pos + a_shift_preliminary - overshoot - result_range::fraction_bits, 0)) : (0);

			constexpr static int a_shift = a_shift_preliminary - overshoot + undershoot;
			constexpr static int b_shift = b_shift_preliminary - overshoot + undershoot;

			// need to find out result-type
			using shifted_a_type = typename A::template scaling_shifted_type<a_shift>;
			using shifted_b_type = typename B::template scaling_shifted_type<b_shift>;

			// need to find out result-range
			using a_range = typename shifted_a_type::range_type;
			using b_range = typename shifted_b_type::range_type;

			using auto_result_range = value_range <
				sub_result_type<a_range, b_range>,
				min_sub_result<a_range, b_range>::value,
				max_sub_result<a_range, b_range>::value
			>;

			using temporary_type = typename auto_result_range::value_type;

			using sub_result_value_type = typename auto_result_range::min_type;
			static constexpr int sub_result_bits = auto_result_range::bits;

			static_assert(shifted_a_type::exponent(0) == shifted_b_type::exponent(0), "My calculations were wrong");
			static_assert(shifted_a_type::low_bits <= max_size && shifted_b_type::low_bits <= max_size, "Could not fit the calculation.");

			static constexpr int result_i = i_constrained ?  result_range::integer_bits  : (sub_result_bits - shifted_a_type::radix_pos);
			static constexpr int result_f = f_constrained ?  result_range::fraction_bits : (shifted_a_type::radix_pos);
			static constexpr int result_o = f_constrained ? (shifted_a_type::radix_pos-result_f) : util::min(shifted_a_type::offset, shifted_b_type::offset);

			using sub_result_type = fixed_auto<result_i, result_f, auto_result_range::is_signed, typename auto_result_range::min_range_type>;
			using add_result_type = fixed<result_i, result_f, RS_sum, result_o>;

			//using sub_result_value_type = typename sub_result_type::value_type;
			using add_result_value_type = typename add_result_type::value_type;

			constexpr static add_result_type add(A a, B b) {
				return add_result_type(a.template scaling_shift<a_shift>().value + b.template scaling_shift<b_shift>().value);
			}

			constexpr static sub_result_type sub(A a, B b) {
				return sub_result_type(temporary_type(a.template scaling_shift<a_shift>().value) - temporary_type(b.template scaling_shift<b_shift>().value));
			}
		};
	}

#define DEBUG_ADD_SUB( A, B, ... ) \
	::fix::detail::add_sub_struct< ::fix::meta::list< __VA_ARGS__ >,  std::decay_t<decltype(A)>, std::decay_t<decltype(B)> >

	template< typename... Args, typename AT, int AI, int AF, bool AS, int AO, typename AR, typename BT, int BI, int BF, bool BS, int BO, typename BR >
	constexpr auto add(fixed<AI, AF, AS, AO, AT, AR> a, fixed<BI, BF, BS, BO, BT, BR> b)
	{
		return detail::add_sub_struct< meta::list<Args...>, decltype(a), decltype(b) >::add(a, b);
	}

	template< typename... Args, typename AT, int AI, int AF, bool AS, int AO, typename AR, typename BT, int BI, int BF, bool BS, int BO, typename BR >
	constexpr auto sub(fixed<AI, AF, AS, AO, AT, AR> a, fixed<BI, BF, BS, BO, BT, BR> b)
	{
		return detail::add_sub_struct< meta::list<Args...>, fixed<AI, AF, AS, AO, AT, AR>, fixed<BI, BF, BS, BO, BT, BR> >::sub(a, b);
	}

	template< typename T >
	constexpr auto integer(T value)
	{
		return fixed<sizeof(T) * 8, 0, std::is_signed<T>::value>(value);
	}

	template< int ConstrainedBits, typename T >
	constexpr auto integer_bits(T value)
	{
		return fixed<ConstrainedBits, 0, std::is_signed<T>::value>(value);
	}

	template<int64 ConstrainedMax, int64 ConstrainedMin=0, typename T>
	constexpr auto integer_range(T value)
	{
		return fixed<util::bits_for_range<int64, ConstrainedMax, ConstrainedMin>::value, 0, std::is_signed<T>::value>(value);
	}

	// default operators (no args to ops)
	template < typename AT, int AI, int AF, bool AS, int AO, typename AR, typename BT, int BI, int BF, bool BS, int BO, typename BR>
	constexpr auto operator+(fixed<AI, AF, AS, AO, AT, AR> a, fixed<BI, BF, BS, BO, BT, BR> b)
	{
		return add<>(a, b);
	}

	template< typename AT, int AI, int AF, bool AS, int AO, typename AR, typename BT, int BI, int BF, bool BS, int BO, typename BR >
	constexpr auto operator-(fixed<AI, AF, AS, AO, AT, AR> a, fixed<BI, BF, BS, BO, BT, BR> b)
	{
		return sub<>(a, b);
	}

	template< typename AT, int AI, int AF, bool AS, int AO, typename AR, typename BT, int BI, int BF, bool BS, int BO, typename BR >
	constexpr auto operator*(fixed<AI, AF, AS, AO, AT, AR> a, fixed<BI, BF, BS, BO, BT, BR> b)
	{
		return mul<>(a, b);
	}

	template< typename FixedType >
	constexpr auto square(FixedType val)
	{
		return mul<positive>(val, val);
	}

}

// internal macro
#define FIXED_RANGE_FROM_VALS(I,F,A,B) \
	::fix::value_range<::fix::detail::value_type_t<(I)+(F), ((A)<0 || (B)<0)>, ::fix::detail::to_fixed<I,F,((A)<0||(B)<0)>(A), ::fix::detail::to_fixed<I,F,((A)<0||(B)<0)>(B)>::min_range_type

// fixed type of reals ranging from A to B with given fraction bits (precision)
#define FIXED_RANGE_P(A,B,Precision) \
	::fix::fixed_auto<::fix::util::range_bits(A,B), (Precision), ((A)<0 || (B)<0), FIXED_RANGE_FROM_VALS(::fix::util::range_bits(A,B), Precision, A, B) >

// fixed type of reals ranging from A to B with given size
#define FIXED_RANGE(A,B,Size) \
	::fix::fixed_auto<::fix::util::range_bits(A,B), (Size-::fix::util::range_bits(A,B)), ((A)<0 || (B)<0), FIXED_RANGE_FROM_VALS(::fix::util::range_bits(A,B), (Size-::fix::util::range_bits(A,B)), A, B) >

// fixed type of integers ranging from A to B
#define FIXED_RANGE_I(A,B) \
	::fix::fixed_auto< ::fix::util::range_bits(A,B), 0, (A<0 || B<0), FIXED_RANGE_FROM_VALS(::fix::util::range_bits(A,B), 0, A, B) >

// Real constant with given fraction bits
#define FIXED_CONSTANT_P(A, Precision) \
	FIXED_RANGE_P(A,A,Precision)::from(A)

// Integer constant
#define FIXED_CONSTANT_I(A) \
	FIXED_RANGE_P(A,A,0)::from(A)

// Real constant with given size
#define FIXED_CONSTANT(A, Size) \
	FIXED_RANGE(A,A,Size)::from(A)

#endif
