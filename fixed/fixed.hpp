//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FIXED_FIXED_HPP__
#define FIXED_FIXED_HPP__

#include <type_traits>
#include <fixed/fixedmath.hpp>
#include <fixed/fixedmeta.hpp>

// mst 2015/07/22
// Todo:
//       - add next_even/next_odd rounding mode
//       - hope people find bugs/fixes ;)

namespace fix {

#define SC static constexpr

	using int64 = long long;
	using uint64 = unsigned long long;

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
	// Value type deduction
	//
	
	namespace detail {
	
		template< typename T, bool Signed = std::is_signed<T>::value >
		struct promoted
		{
			using bare_type =
				util::conditional_t< sizeof(T) == 1, short,
					util::conditional_t< sizeof(T) == 2, int,
						util::conditional_t< sizeof(T) == 4, int64, int64>
					>
				>;

			using type = util::conditional_t< Signed, util::make_signed_t<bare_type>, util::make_unsigned_t<bare_type>>;
		};

		template< typename T, bool Signed = std::is_signed<T>::value >
		using promoted_t = typename promoted<T, Signed>::type;

		template< typename T1, typename T2 >
		struct fitting_type {
			using type = util::conditional_t<  
				sizeof(T1) == sizeof(T2) && !(std::is_signed<T1>::value == std::is_signed<T2>::value),
				promoted_t<T1, true>,
				util::conditional_t<(sizeof(T1)>sizeof(T2)), T1, T2>
			>;
		};

		template< typename T1, typename T2 >
		using fitting_type_t = typename fitting_type<T1, T2>::type;

		template< int Bits, bool Signed >
		struct value_type {
		private:
			static_assert( Bits > 0, "Foooo!");

			using raw_type =
				util::conditional_t< (Bits <= 8), int8,
					util::conditional_t< (Bits <= 16), int16,
						util::conditional_t< (Bits <= 32), int32, int64 >
					>
				>;
		public:
			using type = util::conditional_t<Signed, util::make_signed_t<raw_type>, util::make_unsigned_t<raw_type>>;
		};

		template< int Bits, bool Signed >
		using value_type_t = typename value_type<Bits, Signed>::type;

		template< int I, int F, bool Signed, typename Rounding = rounding::floor, typename T>
		constexpr value_type_t<I + F, Signed> to_fixed(T value)
		{
			return static_cast<value_type_t<I + F, Signed>>(util::scaled_exp2<Rounding::value>(value, F));
		}

	}

	// this one is here for intellisense bug notification.
	template< typename T, T A, T B>
	struct value_range2 {

		SC bool is_signed = util::safe_less(A, 0) || util::safe_less(B, 0);
		SC T minval = util::min(A, B);
		SC T maxval = util::max(A, B);

		static constexpr int  bits = util::range_bits(minval, maxval);

		using value_type = T;
		using min_type = detail::value_type_t<bits, is_signed>;

		template< typename S >
		static constexpr T saturate(S value) {
			return util::saturate(value, minval, maxval);
		}

	};


	template< typename T, T A, T B>
	struct value_range {
		
		SC bool is_signed = util::safe_less(A, 0) || util::safe_less(B, 0);
		SC T minval = util::min(A, B);
		SC T maxval = util::max(A, B);
		
		static constexpr int  bits = util::range_bits(minval, maxval);
		
		using value_type = T;
		using min_type = detail::value_type_t<bits, is_signed>;

		template< typename S >
		static constexpr T saturate(S value) {
			return util::saturate(value, minval, maxval);
		}
	};

	template< typename T >
	struct is_value_range {
		static constexpr bool value = false;
	};

	template< typename T, T A, T B >
	struct is_value_range< value_range<T,A,B> > {
		static constexpr bool value = true;
	};

#define RANGE_FROM_VALS(A,B) \
	value_range<detail::fitting_type_t<decltype(A), decltype(B)>, A, B>

	namespace detail {

		template< int I, int F, bool S >
		using fixed_value_type_t = value_type_t<I+F, S>;
		
		template< int Bits, bool Signed, typename ValueType = value_type_t<Bits, Signed>>
		constexpr ValueType max_value()
		{
			return Signed ? ValueType(util::bitmask<util::make_unsigned_t<ValueType>>(Bits-1)) : ValueType(util::bitmask<ValueType>(Bits));
		}

		template< int Bits, bool Signed, typename ValueType = value_type_t<Bits, Signed> >
		constexpr ValueType min_value()
		{
			return Signed ? ValueType(~util::bitmask<util::make_unsigned_t<ValueType>>(Bits-1)) : ValueType(0);
		}

		template< int Bits, bool Signed, typename ValueType = value_type_t<Bits, Signed> >
		using auto_fixed_range = value_range<ValueType, min_value<Bits, Signed, ValueType>(), max_value<Bits, Signed, ValueType >()>;

		template< int Bits, bool Signed, typename ValueType = value_type_t<Bits, Signed>>
		struct minmax {
			static constexpr ValueType min = min_value<util::min(Bits,1),Signed, ValueType>();
			static constexpr ValueType max = max_value<util::min(Bits,1),Signed, ValueType>();
		};

		template< typename Range, typename Limits >
		struct saturate_range;

		template< typename RangeT, RangeT RangeMin, RangeT RangeMax, typename LimitsT, LimitsT LimitsMin, LimitsT LimitsMax >
		struct saturate_range< value_range<RangeT, RangeMin, RangeMax>, value_range<LimitsT, LimitsMin, LimitsMax> >
		{
			using type = value_range< RangeT, util::saturate(RangeMin, LimitsMin, LimitsMax), util::saturate(RangeMax, LimitsMin, LimitsMax) >;
		};

		template< typename Range, typename Limits >
		using saturate_range_t = typename saturate_range< Range, Limits >::type;

	}

	//
	// Shifting support
	//

	template < int I, int F, bool S = true, typename ValueType = detail::value_type_t<I + F, S>, typename Range = detail::auto_fixed_range<I+F, S> >
	class fixed;

	namespace detail {

		// Handles automatic type promotion while shifting a fixed numer up (<<)
		template< typename OriginalType, int Shift, typename Rounding = rounding::floor >
		struct scaling_shift_values;

		template< int I, int F, bool S, typename ValueType, typename Range, int Shift, typename Rounding >
		struct scaling_shift_values< fixed<I, F, S, ValueType, Range>, Shift, Rounding >
		{
			using original_type = fixed<I, F, S, ValueType, Range>;
			using destination_value_type = value_type_t< I + F + Shift, S >;

			// We can potentially be much smarter here...
			using precast_type = util::conditional_t< (sizeof(destination_value_type)>sizeof(ValueType)) && (Shift > 0), destination_value_type, ValueType >;
			using postcast_type = destination_value_type;

			static constexpr int precast_bits = sizeof(precast_type) * 8;
			static constexpr int free_bits = precast_bits - original_type::data_bits;

			static_assert(I + F + Shift <= precast_bits, "Exceeding platform bounds.");
			static_assert(I + F + Shift > 0, "Shifting to zero.");

			using result_type = fixed<
				I,
				F + Shift,
				S,
				destination_value_type,
				value_range<destination_value_type, util::scaled_exp2<Rounding::value>(precast_type(Range::minval), Shift), util::scaled_exp2<Rounding::value>(precast_type(Range::maxval), Shift)>
			>;
		};

		// Handles automatic type promotion while shifting a fixed numer up (<<)
		template< typename OriginalType, int Shift, typename Rounding = rounding::floor >
		struct literal_shift_values;

		template< int I, int F, bool S, typename ValueType, typename Range, int Shift, typename Rounding >
		struct literal_shift_values< fixed<I, F, S, ValueType, Range>, Shift, Rounding >
		{
			using original_type = fixed<I, F, S, ValueType, Range>;
			using destination_value_type = value_type_t< I + F + Shift, S >;

			// We can potentially be much smarter here...
			using precast_type  = util::conditional_t< (sizeof(destination_value_type)>sizeof(ValueType)) && (Shift > 0), destination_value_type, ValueType >;
			using postcast_type = destination_value_type;

			static constexpr int precast_bits = sizeof(precast_type) * 8;
			static constexpr int free_bits = precast_bits - original_type::data_bits;

			static_assert(I + F + Shift <= precast_bits, "Exceeding platform bounds.");
			static_assert(I + F + Shift > 0, "Shifting to zero.");

			using result_type = fixed<
				I + Shift,
				F,
				S,
				destination_value_type,
				value_range<destination_value_type, util::scaled_exp2<Rounding::value>(precast_type(Range::minval), Shift), util::scaled_exp2<Rounding::value>(precast_type(Range::maxval), Shift)>
			>;
		};

	}

	template< int Shift, typename RoundMode = rounding::floor, int I, int F, bool S, typename T, typename RT >
	constexpr typename detail::scaling_shift_values<fixed<I, F, S, T, RT>, Shift, RoundMode>::result_type
	scaling_shift(fixed<I, F, S, T, RT > val);

	//
	// fixed type
	//

	template<int I, int F, bool S, typename ValueType, typename RangeType>
	class fixed {
	public:
		using value_type  = ValueType;
		using range_type = RangeType;
		
		// some handy constants
		SC bool is_signed = S;

		SC int value_bits = static_cast<int>(sizeof(value_type) * 8);

		// number of integer bits
		SC int integer_bits = I;
		
		// if we're having a signed X.0 type, then >> unsigned bits only leaves the sign in front of the radix point
		SC int unsigned_bits = (is_signed) ? (integer_bits - 1) : integer_bits;

		// number of fractional bits
		SC int fractional_bits = F;
		
		// position of radix point. note that this can very well be negative.
		SC int radix_pos = fractional_bits;

		// number of significant bits
		SC int data_bits = integer_bits + fractional_bits;

		static_assert(data_bits > 0, "Removed all data.");

		// numer of bits used for integer, fractional
		SC int low_bits = data_bits;

		// number of unused bits at the most significant digits
		SC int free_bits = value_bits - low_bits;
		
		// scaling (exp2) which needs to be applied to value to shift the fractional digit before the LSB (ie. to extract the integer-part)
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

		template<int OI, int OF, bool OS, typename OT, typename ORT>
		constexpr fixed(const fixed<OI, OF, OS, OT, ORT>& other)
			: value( static_cast<value_type>(util::scaled_exp2(other.value, F-OF)) )
		{
		}

		template<typename Rounding = rounding::floor, int OI, int OF, bool OS, typename OT, typename OR>
		fixed& assign(fixed<OI, OF, OS, OT, OR> other) {
			value = static_cast<value_type>(scaling_shift<F - OF, Rounding>(other).value);
			return *this;
		}

		template<int OI, int OF, bool OS, typename OT, typename OR>
		fixed& operator=(fixed<OI, OF, OS, OT, OR> other) {
			assign(other);
			return *this;
		}

		template<typename Rounding = rounding::floor, int OI, int OF, bool OS, typename OT, typename OR>
		fixed& assign_saturated(fixed<OI, OF, OS, OT, OR> other) {
			value = range_type::template saturate(scaling_shift<F - OF, Rounding>(other).value);
			return *this;
		}

		template<int OI, int OF, bool OS, typename OT, typename OR>
		fixed& operator*=(fixed<OI, OF, OS, OT, OR> other) {
			assign(*this * other);
			return *this;
		}
		
		template<int OI, int OF, bool OS, typename OT, typename OR>
		fixed& operator+=(fixed<OI, OF, OS, OT, OR> other) {
			assign(*this + other);
			return *this;
		}

		template<int OI, int OF, bool OS, typename OT, typename OR>
		fixed& operator-=(fixed<OI, OF, OS, OT, OR> other) {
			assign(*this - other);
			return *this;
		}

	public:
		static constexpr int exponent(int bit)
		{
			return -fractional_bits + bit;
		}

		static constexpr int max_exponent()
		{
			return exponent(integer_bits + fractional_bits - 1);
		}

		static constexpr int min_exponent()
		{
			return exponent(0);
		}

		template< typename T >
		constexpr auto to_type() const
		{
			return fixed<integer_bits + ((!is_signed && std::is_signed<T>::value) ? 1 : 0), fractional_bits, std::is_signed<T>::value, T, range_type>(value);
		}

		static constexpr fixed max()
		{
			return fixed(static_cast<value_type>(range_type::maxval));
		}

		static constexpr fixed min()
		{
			return fixed(static_cast<value_type>(range_type::minval));
		}

		template< typename RoundingWrapper = rounding::floor, typename U >
		constexpr static fixed from(U value)
		{
			// Need to be more sophisticated here... detect powers of 2 and such which make negative integers or fractions possible (for constants)
			return
				fixed(static_cast<value_type>(util::scaled_exp2<RoundingWrapper::value>(value, -scaling)));
		}

		template<typename U, typename RoundingWrapper = rounding::floor>
		constexpr typename std::enable_if<std::is_floating_point<U>::value, U>::type
			to() const
		{
			return util::scaled_exp2<RoundingWrapper::value, U>(value, scaling);
		}

		template<typename U, typename RoundingWrapper = rounding::floor>
		constexpr typename std::enable_if<std::is_integral<U>::value, U>::type
			to() const
		{
			return util::scaled_exp2<RoundingWrapper::value, U>(value, scaling);
		}

	};

	template< int I, int F, typename RangeType = detail::auto_fixed_range<I + F, false> >
	using ufixed = fixed<I, F, false, detail::value_type_t<I+F, false>, RangeType>;

	template< int I, int F, typename RangeType = detail::auto_fixed_range<I + F, true> >
	using sfixed = fixed<I, F, true, detail::value_type_t<I+F,true>, RangeType>;

	template< int I, int F, bool S, typename R >
	using fixed_auto = ::fix::fixed<I, F, S, ::fix::detail::value_type_t<I + F, S>, R>;

	template< typename T >
	struct is_fixed {
		static constexpr bool value = false;
	};

	template< int I, int F, bool S, typename T, typename RT >
	struct is_fixed< fixed<I, F, S, T, RT > >
	{
		static constexpr bool value = true;
	};

	template< typename T, typename S = void >
	using enable_if_fixed_t = util::enable_if_t< is_fixed<T>::value, S >;
	
	//
	// Operations
	//
	template< int Shift, typename RoundMode, int I, int F, bool S, typename T, typename RT >
	constexpr typename detail::scaling_shift_values<fixed<I, F, S, T, RT>, Shift, RoundMode>::result_type
	scaling_shift(fixed<I, F, S, T, RT > val)
	{
		using values = detail::scaling_shift_values<fixed<I, F, S, T, RT>, Shift, RoundMode>;
		using return_type = typename values::result_type;
		using return_value_type = typename return_type::value_type;
		using precast_type = typename values::precast_type;

		return return_type(
			static_cast<return_value_type>(
				util::scaled_exp2<RoundMode::value>(
					static_cast<precast_type>(val.value),
					Shift
				)
			)
		);
	}

	template< int Shift, typename RoundMode = rounding::floor, int I, int F, bool S, typename T, typename RT >
	constexpr typename detail::scaling_shift_values<fixed<I, F, S, T, RT>, Shift, RoundMode>::result_type
		sshift(fixed<I, F, S, T, RT > val)
	{
		return scaling_shift<Shift, RoundMode>(val);
	}

	template< int Shift, int I, int F, bool S, typename T, typename RT >
	constexpr fixed<I + Shift, F - Shift, S, T, RT>
	virtual_shift(fixed<I, F, S, T, RT> val)
	{
		return fixed<I + Shift, F - Shift, S, T, RT>(val.value);
	}

	template< int Shift, int I, int F, bool S, typename T, typename RT >
	constexpr fixed<I + Shift, F - Shift, S, T, RT>
		vshift(fixed<I, F, S, T, RT> val)
	{
		return fixed<I + Shift, F - Shift, S, T, RT>(val.value);
	}


	template< int Shift, typename RoundMode = rounding::floor, int I, int F, bool S, typename T, typename RT >
	constexpr typename detail::literal_shift_values<fixed<I, F, S, T, RT>, Shift, RoundMode>::result_type
	literal_shift(fixed<I, F, S, T, RT> val)
	{
		using values = detail::literal_shift_values<fixed<I, F, S, T, RT>, Shift, RoundMode>;
		using return_type = typename values::result_type;
		using precast_type = typename values::precast_type;

		return return_type(
			util::scaled_exp2<RoundMode::value>(
				static_cast<precast_type>(val.value),
				Shift
			)
		);
	}

	template< int Shift, typename RoundMode = rounding::floor, int I, int F, bool S, typename T, typename RT >
	constexpr typename detail::literal_shift_values<fixed<I, F, S, T, RT>, Shift, RoundMode>::result_type
		lshift(fixed<I, F, S, T, RT> val)
	{
		return literal_shift<Shift, RoundMode>(val);
	}

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
			using rounding_type = typename meta::find_if_or< ArgList, meta::integral_constant_finder<RoundModes>, rounding::floor >::type;

			// Check for maximum bitcount in intermediates
			static constexpr int max_size = meta::find_if_or< ArgList, max_size_finder, platform_max_size>::type::value;

			static_assert(max_size <= platform_max_size::value, "You are exceeding your platform's maximum bitcount.");
		};

		template< typename A, typename B >
		struct auto_type {
			using intermediate = util::conditional_t< (sizeof(A) >= sizeof(B)), A, B >;
			using type = 
				util::conditional_t< std::is_signed<A>::value || std::is_signed<B>::value, util::make_signed_t<intermediate>, util::make_unsigned_t<intermediate>>;
		};

		template< typename Args, typename Nom, typename Denom >
		struct div_struct;

		template< typename Args, typename NomT, int NomI, int NomF, bool NomS, typename NomR, typename DenT, int DenI, int DenF, bool DenS, typename DenR >
		struct div_struct< Args, fixed<NomI,NomF,NomS, NomT, NomR>, fixed<DenI, DenF, DenS, DenT, DenR>>  {
			using nom_type = fixed<NomI, NomF, NomS, NomT, NomR>;
			using den_type = fixed<DenI, DenF, DenS, DenT, DenR>;
			using parsed_args = div_args<Args>;

			using rounding_type = typename parsed_args::rounding_type;
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
			static constexpr int shift_nom =  util::min<int>(shift_nom_exact, max_size - nom_type::fractional_bits - nom_type::integer_bits);
			static constexpr int shift_den = -util::max<int>(shift_nom_exact-shift_nom, 0);
			
			static constexpr result_type divide(nom_type nom, den_type den)
			{
				return result_type(
					static_cast<result_value_type>(scaling_shift<shift_nom, rounding_type>(nom).value / scaling_shift<shift_den, rounding_type>(den).value )
					);
			}
		};
	}

	template< typename... Args, typename NomType, typename DenType, typename Test = util::enable_if_t<is_fixed<NomType>::value && is_fixed<DenType>::value>>
	constexpr auto /*typename detail::div_struct<meta::list<Args...>, NomType, DenType>::result_type*/
		div(NomType nom, DenType den)
	{
		return detail::div_struct<meta::list<Args...>, NomType, DenType>::divide(nom, den);
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
			using result_range = meta::find_if_or_t< ArgList, fits_finder, fits<std::numeric_limits<int>::max()> >;
			static constexpr bool constrained_integer = (result_range::integer_bits != std::numeric_limits<int>::max());
			static constexpr bool constrained_fraction = (result_range::fraction_bits != std::numeric_limits<int>::max());

			// Check for rounding mode
			using rounding_type = meta::find_if_or_t< ArgList, meta::integral_constant_finder<RoundModes>, rounding::floor >;

			// Check for maximum bitcount in intermediates
			static constexpr int max_size = meta::find_if_or_t< ArgList, max_size_finder, platform_max_size>::value;

			static_assert(max_size <= platform_max_size::value, "You are exceeding your platform's maximum bitcount.");
		};

		template< typename RangeA, typename RangeB >
		using mul_result_type_t =
			detail::value_type_t<
				(sizeof(typename RangeA::value_type) + sizeof(typename RangeB::value_type)) * 8,
				std::is_signed<typename RangeA::value_type>::value || std::is_signed<typename RangeB::value_type>::value
			>;

		template< typename RangeA, typename RangeB >
		struct max_mul_result {
			using result_type = mul_result_type_t<RangeA, RangeB>;

			SC result_type value = 
				util::max(
					util::max( result_type(RangeA::maxval) * result_type(RangeB::maxval), result_type(RangeA::maxval) * result_type(RangeB::minval) ),
					util::max( result_type(RangeA::minval) * result_type(RangeB::maxval), result_type(RangeA::minval) * result_type(RangeB::minval) )
				);
		};

		template< typename RangeA, typename RangeB >
		struct min_mul_result {
			using result_type = mul_result_type_t<RangeA, RangeB>;

			SC result_type value = 
				util::min(
					util::min( result_type(RangeA::maxval) * result_type(RangeB::maxval), result_type(RangeA::maxval) * result_type(RangeB::minval) ),
					util::min( result_type(RangeA::minval) * result_type(RangeB::maxval), result_type(RangeA::minval) * result_type(RangeB::minval) )
				);
		};

		template< typename RangeA, typename RangeB >
		using mul_result_range_t = value_range< mul_result_type_t<RangeA, RangeB>, min_mul_result<RangeA, RangeB>::value, max_mul_result<RangeA, RangeB>::value >;

		template< typename Args, typename Nom, typename Denom >
		struct mul_struct;

		template< typename Args, typename AT, int AI, int AF, bool AS, typename AR, typename BT, int BI, int BF, bool BS, typename BR >
		struct mul_struct< Args, fixed<AI, AF, AS, AT, AR>, fixed<BI, BF, BS, BT, BR>> {
			using a_type = fixed<AI, AF, AS, AT, AR>;
			using b_type = fixed<BI, BF, BS, BT, BR>;
			using parsed_args = mul_args<Args>;
			
			// the result range possibly specified by fits<A(,B)>
			using result_range = typename parsed_args::result_range;

			// the automatic result value type, if not otherwise constrained by user
			using result_value_type = typename auto_type<AT, BT>::type;
			using range = mul_result_range_t<AR, BR>;
			using intermediate_type = typename range::min_type;

			using rounding_type = typename parsed_args::rounding_type;

			static constexpr bool assume_positive = parsed_args::assume_result_positive;

			//static constexpr int  auto_i = AI + BI;
			static constexpr bool stripped_sign = (AS || BS) && assume_positive;
			static constexpr bool auto_s = (AS || BS) && (!assume_positive);

			SC int auto_i = range::bits - AF - BF - (stripped_sign ? 1 : 0);

			static constexpr int result_i = parsed_args::constrained_integer ? result_range::integer_bits : auto_i;
			static constexpr int auto_f   = int(sizeof(result_value_type) * 8) - result_i;
			static constexpr int result_f = parsed_args::constrained_fraction ? result_range::fraction_bits : auto_f;

			static constexpr int max_size = parsed_args::max_size;
	
			// Check to see if temporary overshoots the maximum given bitcount
			static constexpr int overshoot = util::max(AI + AF + BI + BF - max_size, 0);
			
			static constexpr int a_shift_temp = -util::min(0, overshoot);
			static constexpr int b_shift_temp = -util::min(0, overshoot + a_shift_temp);
			
			static constexpr int remaining_overshoot = util::max(overshoot + a_shift_temp + b_shift_temp, 0);
			static constexpr int a_shift = a_shift_temp - remaining_overshoot / 2;
			static constexpr int b_shift = b_shift_temp - remaining_overshoot / 2 - remaining_overshoot % 2;
			
			using scaling_a_values = detail::scaling_shift_values<a_type, a_shift, rounding_type>;
			using scaling_b_values = detail::scaling_shift_values<b_type, b_shift, rounding_type>;

			using shifted_a_type = typename scaling_a_values::result_type;
			using shifted_b_type = typename scaling_b_values::result_type;

			static_assert((shifted_a_type::low_bits + shifted_b_type::low_bits) <= max_size, "Overshooting temporary.");

			using mul_result_value_type = value_type_t<shifted_a_type::low_bits + shifted_b_type::low_bits, auto_s>;
			static constexpr int mul_result_radix_pos = shifted_a_type::radix_pos + shifted_b_type::radix_pos;
			static constexpr int result_radix_pos = result_f;

			static constexpr int result_shift = result_radix_pos - mul_result_radix_pos;

			using range_value_type = typename range::value_type;
			static constexpr auto result_min = (assume_positive && range::minval < 0) ? range_value_type(0) : (util::shifted(range::minval, result_shift));
			static constexpr auto result_max = util::shifted(range::maxval, result_shift);

			using result_range_type = value_range<range_value_type, result_min, result_max>;

			using result_type = fixed_auto<result_i, result_f, auto_s && !assume_positive, result_range_type>;

			// should not assert ever, at least if the user doesn't give I and F and a max_size which doesn't fit them.
			static_assert(result_i + result_f <= max_size, "Cannot multiply with given constraints.");

			static constexpr result_type mul(a_type a, b_type b)
			{
				return result_type(
					static_cast<typename result_type::value_type>(
						util::scaled_exp2<rounding_type::value>(
							intermediate_type(scaling_shift<a_shift, rounding_type>(a).value) * intermediate_type(scaling_shift<b_shift, rounding_type>(b).value),
							result_shift)
						) 
				);
			}
		};

	}

	template< typename... Args, typename AT, typename BT, typename Test = util::enable_if_t< is_fixed<AT>::value && is_fixed<BT>::value>>
	constexpr auto /* typename detail::mul_struct<meta::list<Args...>, AT, BT>::result_type */
		mul(AT a, BT b)
	{
		return detail::mul_struct<meta::list<Args...>, AT, BT>::mul(a, b);
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
			using result_range = meta::find_if_or_t< ArgList, fits_finder, fits<std::numeric_limits<int>::max()> >;
			static constexpr bool is_integer_constrained  = (result_range::integer_bits != std::numeric_limits<int>::max());
			static constexpr bool is_fraction_constrained = (result_range::fraction_bits != std::numeric_limits<int>::max());

			static constexpr int constrained_integer_bits  = result_range::integer_bits;
			static constexpr int constrained_fraction_bits = result_range::fraction_bits;

			// Check for rounding mode
			using rounding_type = meta::find_if_or_t< ArgList, meta::integral_constant_finder<RoundModes>, rounding::floor >;

			// Check for maximum bitcount in intermediates
			static constexpr int max_size = meta::find_if_or< ArgList, max_size_finder, platform_max_size>::type::value;
			static constexpr bool max_size_constrained = !std::is_same< meta::find_if_t<ArgList, max_size_finder>, meta::void_type >::value;

			static_assert(max_size <= platform_max_size::value, "You are exceeding the platform's maximum bitcount.");
		};

		template< typename RangeA, typename RangeB >
		struct sub_result_value_type;

		template< typename AT, AT amin, AT amax, typename BT, BT bmin, BT bmax >
		struct sub_result_value_type< value_range<AT, amin, amax>, value_range<BT, bmin, bmax> > {
			using a_range = value_range<AT, amin, amax>;
			using b_range = value_range<BT, bmin, bmax>;
			
			using type = detail::value_type_t<
				util::max<int>(util::range_bits(amin, amax), util::range_bits(bmin, bmax)),
				util::safe_less(a_range::minval, b_range::maxval)
			>;
		};
		
		template< typename RangeA, typename RangeB >
		using sub_result_value_type_t = typename sub_result_value_type<RangeA, RangeB>::type;

		template< typename RangeA, typename RangeB >
		struct sub_temporary_value_type;

		template< typename AT, AT amin, AT amax, typename BT, BT bmin, BT bmax >
		struct sub_temporary_value_type< value_range<AT, amin, amax>, value_range<BT, bmin, bmax> > {
			using a_range = value_range<AT, amin, amax>;
			using b_range = value_range<BT, bmin, bmax>;

			using type = detail::value_type_t<
				util::max<int>(util::range_bits(amin, amax), util::range_bits(bmin, bmax)) + 1,
				util::safe_less(a_range::minval, b_range::maxval)
			>;
		};

		template< typename RangeA, typename RangeB >
		using sub_temporary_value_type_t = typename sub_temporary_value_type<RangeA, RangeB>::type;
			
		template< typename RangeA, typename RangeB >
		struct max_sub_result {
			static_assert(is_value_range<RangeA>::value && is_value_range<RangeB>::value, "No value range!");

			using res_type = sub_result_value_type_t<RangeA, RangeB>;
			using temp_type = sub_temporary_value_type_t<RangeA, RangeB>;

			SC res_type value = static_cast<res_type>(
				util::max(
					util::max( temp_type(RangeA::maxval) - temp_type(RangeB::maxval), temp_type(RangeA::maxval) - temp_type(RangeB::minval) ),
					util::max( temp_type(RangeA::minval) - temp_type(RangeB::maxval), temp_type(RangeA::minval) - temp_type(RangeB::minval) )
				)
			);
		};

		template< typename RangeA, typename RangeB>
		struct min_sub_result {
			static_assert(is_value_range<RangeA>::value && is_value_range<RangeB>::value, "No value range!");

			using res_type  = sub_result_value_type_t<RangeA, RangeB>;
			using temp_type = sub_temporary_value_type_t<RangeA, RangeB>;

			SC res_type value = static_cast<res_type>(
				util::min(
					util::min( temp_type(RangeA::maxval) - temp_type(RangeB::maxval), temp_type(RangeA::maxval) - temp_type(RangeB::minval) ),
					util::min( temp_type(RangeA::minval) - temp_type(RangeB::maxval), temp_type(RangeA::minval) - temp_type(RangeB::minval) )
				)
			);
		};

		template< typename RangeA, typename RangeB >
		struct add_result_value_type;

		template< typename AT, AT amin, AT amax, typename BT, BT bmin, BT bmax >
		struct add_result_value_type< value_range<AT, amin, amax>, value_range<BT, bmin, bmax> > {
			using type = value_type_t<
				util::max(
					util::range_bits(amin, amax),
					util::range_bits(bmin ,bmax)
				),
				util::is_neg(amin) || util::is_neg(amax) || util::is_neg(bmin) || util::is_neg(bmax)
			>;
		};

		template< typename RangeA, typename RangeB >
		using add_result_value_type_t = typename add_result_value_type<RangeA,RangeB>::type;

		template< typename RangeA, typename RangeB >
		struct add_temporary_value_type;

		template< typename AT, AT amin, AT amax, typename BT, BT bmin, BT bmax >
		struct add_temporary_value_type< value_range<AT, amin, amax>, value_range<BT, bmin, bmax> > {
			using type = value_type_t<
				util::max(
					util::range_bits(amin, amax),
					util::range_bits(bmin, bmax)
					) + 1,
				util::is_neg(amin) || util::is_neg(amax) || util::is_neg(bmin) || util::is_neg(bmax)
			>;
		};

		template< typename RangeA, typename RangeB >
		using add_temporary_value_type_t = typename add_temporary_value_type<RangeA, RangeB>::type;

		template< typename RangeA, typename RangeB>
		struct max_add_result {
			using res_type  = add_result_value_type_t   <RangeA, RangeB>;
			using temp_type = add_temporary_value_type_t<RangeA, RangeB>;

			SC res_type value = static_cast<res_type>(
				util::max(
					util::max(temp_type(RangeA::maxval) + temp_type(RangeB::maxval), temp_type(RangeA::maxval) + temp_type(RangeB::minval)),
					util::max(temp_type(RangeA::minval) + temp_type(RangeB::maxval), temp_type(RangeA::minval) + temp_type(RangeB::minval))
					)
				);
		};

		template< typename RangeA, typename RangeB >
		struct min_add_result {
			using res_type = add_result_value_type_t<RangeA, RangeB>;
			using temp_type = add_temporary_value_type_t<RangeA, RangeB>;

			SC res_type value = static_cast<res_type>(
				util::min(
					util::min(temp_type(RangeA::maxval) + temp_type(RangeB::maxval), temp_type(RangeA::maxval) + temp_type(RangeB::minval)),
					util::min(temp_type(RangeA::minval) + temp_type(RangeB::maxval), temp_type(RangeA::minval) + temp_type(RangeB::minval))
					)
				);
		};

		template< typename ArgList, typename A, typename B >
		struct add_sub_struct;

		// Find the best combination of shifts so the fractional points are aligned,
		// minimizing shift operations but only if no precision is lost
		template< typename ArgList, int AI, int AF, bool AS, typename AT, typename AR, int BI, int BF, bool BS, typename BT, typename BR >
		struct add_sub_struct< ArgList, fixed<AI, AF, AS, AT, AR>, fixed<BI, BF, BS, BT, BR> >
		{
			// This is a mess!
			using A = fixed<AI, AF, AS, AT, AR>;
			using B = fixed<BI, BF, BS, BT, BR>;

			using parsed_args = add_sub_args< ArgList >;

			using rounding_type = typename parsed_args::rounding_type;

			static constexpr int max_size = parsed_args::max_size_constrained ? parsed_args::max_size : util::max(sizeof(AT),sizeof(BT))*8;
			using result_range = typename parsed_args::result_range;

			constexpr static bool i_constrained = parsed_args::is_integer_constrained;
			constexpr static bool f_constrained = parsed_args::is_fraction_constrained;

			constexpr static int  i_constrained_bits = parsed_args::constrained_integer_bits;
			constexpr static int  f_constrained_bits = parsed_args::constrained_fraction_bits;

			// Determine lhs and rhs scaling shifts to match radix points
			constexpr static int exponent_difference = B::fractional_bits - A::fractional_bits;

			constexpr static int a_shift_preliminary = f_constrained ? (f_constrained_bits - A::fractional_bits) : util::max( exponent_difference, 0);
			constexpr static int b_shift_preliminary = f_constrained ? (f_constrained_bits - B::fractional_bits) : util::max(-exponent_difference, 0);

			constexpr static bool RS_sum = parsed_args::assume_result_positive ? false : (AS || BS);
			// if not otherwise expressed by an argument, always assume signed destination range for subtraction
			constexpr static bool RS_sub = parsed_args::assume_result_positive ? false : true;

			constexpr static int a_sign_extension = (!AS && BS) ? 1 : 0;
			constexpr static int b_sign_extension = (!BS && AS) ? 1 : 0;

			using a_extended_type = util::conditional_t<(!AS && BS), util::make_signed_t<typename A::value_type>, typename A::value_type>;
			using b_extended_type = util::conditional_t<(!BS && AS), util::make_signed_t<typename B::value_type>, typename B::value_type>;
			
			// If either side is overshooting the maximum bit size, shift both down accordingly
			constexpr static int overshoot  = util::max(util::max((A::low_bits + a_shift_preliminary + a_sign_extension) - max_size, (B::low_bits + b_shift_preliminary + b_sign_extension) - max_size), 0);

			// If custom-dest fraction bits lead to a negative offset, shift both up accordingly 
			//constexpr static int undershoot = f_constrained ? (-util::min(A::radix_pos + a_shift_preliminary - overshoot - result_range::fraction_bits, 0)) : (0);

			constexpr static int a_shift = a_shift_preliminary - overshoot /*+ undershoot*/;
			constexpr static int b_shift = b_shift_preliminary - overshoot /*+ undershoot*/;

			// need to find out result-type
			using scaling_a_values = detail::scaling_shift_values<A, a_shift, rounding_type>;
			using scaling_b_values = detail::scaling_shift_values<B, b_shift, rounding_type>;

			using shifted_a_type = typename scaling_a_values::result_type;
			using shifted_b_type = typename scaling_b_values::result_type;

			static_assert(shifted_a_type::exponent(0) == shifted_b_type::exponent(0), "My calculations were wrong");
			static_assert(shifted_a_type::low_bits <= max_size && shifted_b_type::low_bits <= max_size, "Could not fit the calculation.");


			// need to find out result-range
			using shifted_a_range = typename shifted_a_type::range_type;
			using shifted_b_range = typename shifted_b_type::range_type;

			using auto_sub_result_range = value_range <
				sub_result_value_type_t<shifted_a_range, shifted_b_range>,
				min_sub_result<shifted_a_range, shifted_b_range>::value,
				max_sub_result<shifted_a_range, shifted_b_range>::value
			>;

			using auto_add_result_range = value_range <
				add_result_value_type_t<shifted_a_range, shifted_b_range>,
				min_add_result<shifted_a_range, shifted_b_range>::value,
				max_add_result<shifted_a_range, shifted_b_range>::value
			>;

			// Don't eat up more bits than the operands
			using max_add_range = auto_fixed_range<util::min<int>(util::max<int>(shifted_a_range::bits, shifted_b_range::bits), auto_add_result_range::bits), auto_add_result_range::is_signed>;
			using max_sub_range = auto_fixed_range<util::min<int>(util::max<int>(shifted_a_range::bits, shifted_b_range::bits), auto_sub_result_range::bits), auto_sub_result_range::is_signed>;

			// Saturate calculated ranges into the limited result ranges
			using sub_result_range = detail::saturate_range_t<auto_sub_result_range, max_sub_range>;
			using sub_temporary_value_type = typename sub_result_range::value_type;

			using add_result_range = detail::saturate_range_t<auto_add_result_range, max_add_range>;
			using add_temporary_value_type = typename add_result_range::value_type;

			using sub_result_value_type = sub_temporary_value_type;
			static constexpr int sub_result_bits = sub_result_range::bits;

			using add_result_value_type = sub_temporary_value_type;
			static constexpr int add_result_bits = add_result_range::bits;

			static constexpr int sub_i = i_constrained ?  result_range::integer_bits  : (sub_result_bits - shifted_a_type::radix_pos);
			static constexpr int sub_f = f_constrained ?  result_range::fraction_bits : (shifted_a_type::radix_pos);
		
			static constexpr int add_i = i_constrained ?  result_range::integer_bits  : (add_result_bits - shifted_a_type::radix_pos);
			static constexpr int add_f = f_constrained ?  result_range::fraction_bits : (shifted_a_type::radix_pos);
			
			using sub_result_type = fixed_auto<sub_i, sub_f, sub_result_range::is_signed, sub_result_range>;
			using add_result_type = fixed_auto<add_i, add_f, add_result_range::is_signed, add_result_range>;

			constexpr static sub_result_type sub(A a, B b) {
				return sub_result_type(sub_temporary_value_type(scaling_shift<a_shift, rounding_type>(a).value) - sub_temporary_value_type(scaling_shift<b_shift, rounding_type>(b).value));
			}

			constexpr static add_result_type add(A a, B b) {
				return add_result_type(add_temporary_value_type(scaling_shift<a_shift, rounding_type>(a).value) + add_temporary_value_type(scaling_shift<b_shift, rounding_type>(b).value));
			}
		};
	}

#define DEBUG_ADD_SUB( A, B, ... ) \
	::fix::detail::add_sub_struct< ::fix::meta::list< __VA_ARGS__ >,  std::decay_t<decltype(A)>, std::decay_t<decltype(B)> >

	template< typename... Args, typename AT, typename  BT, typename Test = util::enable_if_t<is_fixed<AT>::value && is_fixed<BT>::value> >
	constexpr auto /* typename detail::add_sub_struct< meta::list<Args...>, AT, BT >::add_result_type */
		add(AT a, BT b)
	{
		return detail::add_sub_struct< meta::list<Args...>, AT, BT >::add(a, b);
	}

	template< typename... Args, typename AT, typename BT, typename Test = util::enable_if_t<is_fixed<AT>::value && is_fixed<BT>::value>>
	constexpr auto /*typename detail::add_sub_struct< meta::list<Args...>, AT, BT >::sub_result_type*/
		sub(AT a, BT b)
	{
		return detail::add_sub_struct< meta::list<Args...>, AT, BT >::sub(a, b);
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
		return fixed<util::range_bits(ConstrainedMax, ConstrainedMin), 0, std::is_signed<T>::value>(value);
	}

	// default operators (no args to ops)
	template< typename AT, typename  BT, typename Test = util::enable_if_t<is_fixed<AT>::value && is_fixed<BT>::value>>
	constexpr auto /*typename detail::add_sub_struct< meta::list<>, AT, BT >::add_result_type*/
		operator+(AT a, BT b)
	{
		return add<>(a, b);
	}

	template< typename AT, typename  BT, typename Test = util::enable_if_t<is_fixed<AT>::value && is_fixed<BT>::value>>
	constexpr auto /*typename detail::add_sub_struct< meta::list<>, AT, BT >::sub_result_type*/
		operator-(AT a, BT b)
	{
		return sub<>(a, b);
	}

	template< typename AT, typename  BT, typename Test = util::enable_if_t<is_fixed<AT>::value && is_fixed<BT>::value> >
	constexpr auto /*typename detail::mul_struct< meta::list<>, AT, BT >::result_type*/
	operator*(AT a, BT b)
	{
		return mul<>(a, b);
	}

	template< typename FixedType, typename Test = util::enable_if_t< is_fixed<FixedType>::value >  >
	constexpr auto /*typename detail::mul_struct< meta::list<positive>, FixedType, FixedType >::result_type*/
	square(FixedType val)
	{
		return mul<positive>(val, val);
	}

}

// internal macro
#define FIXED_RANGE_FROM_VALS(I,F,A,B) \
	::fix::value_range<::fix::detail::value_type_t<(I)+(F), ::fix::util::any_neg(A,B)>, ::fix::detail::to_fixed<I,F,::fix::util::any_neg(A,B), ::fix::rounding::floor>(::fix::util::mixed_min(A,B)), ::fix::detail::to_fixed<I,F,::fix::util::any_neg(A,B), ::fix::rounding::ceil>(::fix::util::mixed_max(A,B))>

// fixed type of reals ranging from A to B with given fraction bits (precision)
#define FIXED_RANGE_P(A,B,Precision) \
	::fix::fixed_auto<::fix::util::range_bits(A,B), (Precision), ::fix::util::any_neg(A,B), FIXED_RANGE_FROM_VALS(::fix::util::range_bits(A,B), Precision, A, B) >

// fixed type of reals ranging from A to B with given size
#define FIXED_RANGE(A,B,Size) \
	::fix::fixed_auto<::fix::util::range_bits(A,B), (Size-::fix::util::range_bits(A,B)), ::fix::util::any_neg(A,B), FIXED_RANGE_FROM_VALS(::fix::util::range_bits(A,B), (Size-::fix::util::range_bits(A,B)), A, B) >

// fixed type of integers ranging from A to B
#define FIXED_RANGE_I(A,B) \
	::fix::fixed_auto< ::fix::util::range_bits(A,B), 0, ::fix::util::any_neg(A,B), FIXED_RANGE_FROM_VALS(::fix::util::range_bits(A,B), 0, A, B) >

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
