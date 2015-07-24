//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <fixed/fixed.hpp>

template< int I >
constexpr auto sine_normalize(fix::sfixed<I, 0> angle)
{
	return angle; //(((angle.value << 1) ^ angle.value) < 0) ? (fix::sfixed<I, 0>((1 << (I - 1)) - angle)) : angle;
}

template< typename FixedType >
constexpr auto sine3_1stq(FixedType angle)
{
	// "taylor": 1/2 * angle * (3 - angle*angle)
	return (angle * (FIXED_CONSTANT_I(3) - fix::square(angle))).template virtual_shift<-1>();
}

template< typename FixedType >
constexpr auto sine3(FixedType angle)
{
	return sine3_1stq(angle);
}

#define CA constexpr auto

void sine_test2()
{
	using namespace fix;
	CA angle = FIXED_RANGE(-1.0, 1.0, 16)::from(0.1);
	CA angle_integer = angle.integer_bits;
	
	CA angle_max = angle.max().to<double>();
	CA angle_min = angle.min().to<double>();

	CA angle_norm = angle.virtual_shift<0>();

	CA angle_norm_max = angle_norm.max().to<double>();
	CA angle_norm_min = angle_norm.min().to<double>();

	CA squared = square(angle_norm);

	CA squared_max = squared.max().to<double>();
	CA squared_min = squared.min().to<double>();
	
	CA value = FIXED_CONSTANT_I(3) - fix::square(angle_norm);
	CA integer = value.integer_bits;
	CA fraction = value.fractional_bits;
	CA diff_min = value.min().to<double>();
	CA diff_max = value.max().to<double>();

	CA result = (angle_norm * value).virtual_shift<-1>();
	CA result_integer = result.integer_bits;
	CA result_fraction = result.fractional_bits;
	CA result_max = result.max();
	CA result_max_val = result_max.to<double>();
	CA result_min = result.min();
	CA result_min_val = result_min.to<double>();

	CA angle_val = angle_norm.to<double>() * 90;
	CA val = sine3(angle);
	CA result_bits = val.integer_bits;
	CA conv = val.to<double>();
}

void range_mul_test()
{
	using namespace fix;
	CA angle = sfixed<12, 0, value_range<short, -2047, 2047>>::from(-2047).virtual_shift<-11>();
	CA product = angle * angle * angle * angle;
	CA value = product.to<double>();
}



void bits_for_range_test()
{
	using namespace fix;

	static_assert(util::bits_for_range<unsigned short,      0, 65535>::value == 16, "Fooo!");
	static_assert(util::bits_for_range<         short, -32768, 32767>::value == 16, "Fooo!");
	static_assert(util::bits_for_range<           int, -32768, 32768>::value == 17, "Fooo!");
	
	static_assert(detail::auto_fixed_range< 1, true>::bits == 1, "Foo!");
	static_assert(detail::auto_fixed_range< 2, true>::bits == 2, "Foo!");
	static_assert(detail::auto_fixed_range< 3, true>::bits == 3, "Foo!");
	static_assert(detail::auto_fixed_range< 8, true>::bits == 8, "Foo!");
	static_assert(detail::auto_fixed_range<16, true>::bits == 16, "Foo!");
	static_assert(detail::auto_fixed_range<32, true>::bits == 32, "Foo!");

	CA bits1 = util::bits_for_range<short, -16192, 0>::value;

	using range_type = RANGE_FROM_VALS(-3232, 5136);
	CA bits = range_type::bits;
	using min_type = range_type::min_type;
	constexpr min_type Foo = 233;

	using my_type = FIXED_RANGE_I(-32768, 32767);

	CA minval = my_type::range_type::min;
	CA maxval = my_type::range_type::max;

	//CA asdga = ::fix::util::integer_bits_interval(64.4231, -64.4231);
	
	CA intbits = util::integer_bits(68000.4231);

	CA constant = FIXED_CONSTANT_I(68000);
	CA bits2 = decltype(constant)::range_type::bits;
	CA F = decltype(constant)::fractional_bits;

	CA value = constant.to<double>();

}

void storage_type_test() {
	constexpr fix::sfixed<31, 0> value_int32(0);
	static_assert(std::is_same< decltype(value_int32)::value_type, int >::value, "Wrong storage type");

	constexpr fix::ufixed<16, 0> value_uint16(0);
	static_assert(std::is_same< decltype(value_uint16)::value_type, unsigned short >::value, "Wrong storage type");
}

void macro_test()
{
	using namespace fix;
	
	CA val2 = util::range_bits(32767, -32768);

	constexpr auto size_value = FIXED_CONSTANT(2.45, 32);	// Given result size
	constexpr auto prec_value = FIXED_CONSTANT_P(2.45, 8);	// Given result precision

	using range_p_value_type = FIXED_RANGE_P(-2.45, 10.45, 5);

	CA value = unsigned short(util::abs(short(-32768)));

	CA bits3 = util::integer_bits(32768);

	CA bits  = util::range_bits(-2.45, 10.45);
	CA bits2 = util::range_bits(32767, -32768);

	CA isneg = ::fix::util::any_neg(-2.45, 10.45);

	using range_type = ::fix::value_range<::fix::detail::value_type_t<(bits)+(32 - bits), ::fix::util::any_neg(-2.45, 10.45)>, ::fix::detail::to_fixed<bits, 32 - bits, ::fix::util::any_neg(-2.45, 10.45)>(-2.45), ::fix::detail::to_fixed<bits, 32 - bits, ::fix::util::any_neg(-2.45, 10.45)>(10.45)>::min_range_type;

	using range_s_value_type = FIXED_RANGE(-2.45, 10.45, 32);

	constexpr auto conv1 = size_value.to<double>();
	constexpr auto conv2 = prec_value.to<double>();
}

template< typename FixedA, typename FixedB >
constexpr auto max_mul_integer_bits(FixedA a, FixedB b)
{

}

void test_min_max()
{
	using namespace fix;
	using type_a = sfixed<32, 0>;
	using type_b = sfixed<32, 0>;
	
	using result_type = detail::value_type_t<type_a::data_bits + type_b::data_bits, type_a::is_signed || type_b::is_signed>;

	CA amax = type_a::max();
	CA amin = type_a::min();

	CA bmax = type_b::max();
	CA bmin = type_b::min();

	CA maxresult = util::max(
		result_type(type_a::max().value) * type_b::max().value,
		result_type(type_a::max().value) * type_b::min().value,
		result_type(type_a::min().value) * type_b::max().value,
		result_type(type_a::min().value) * type_b::min().value 
		);

	CA minresult = util::min(
		result_type(type_a::max().value) * type_b::max().value,
		result_type(type_a::max().value) * type_b::min().value,
		result_type(type_a::min().value) * type_b::max().value,
		result_type(type_a::min().value) * type_b::min().value
		);

	CA teasa = util::log2_ceil(util::abs(minresult));
	CA bits = util::max( util::log2_ceil(maxresult), util::log2_ceil(minresult) );
}


void from_floating_rounding_test()
{
	using namespace fix;
	using type = sfixed<4, 0>;

	// ceil
	{
		static_assert(type::from<rounding::ceil>(4.0).to<int>() == 4, "Bad ceil rounding");
		static_assert(type::from<rounding::ceil>(-4.0).to<int>() == -4, "Bad ceil rounding");
		static_assert(type::from<rounding::ceil>(4.1).to<int>() == 5, "Bad ceil rounding");
		static_assert(type::from<rounding::ceil>(-4.1).to<int>() == -4, "Bad ceil rounding");
		static_assert(type::from<rounding::ceil>(4.5).to<int>() == 5, "Bad ceil rounding");
		static_assert(type::from<rounding::ceil>(-4.5).to<int>() == -4, "Bad ceil rounding");
		static_assert(type::from<rounding::ceil>(4.7).to<int>() == 5, "Bad ceil rounding");
		static_assert(type::from<rounding::ceil>(-4.7).to<int>() == -4, "Bad ceil rounding");
	}

	// floor
	{
		static_assert(type::from<rounding::floor>(4.0).to<int>() == 4, "Bad floor rounding");
		static_assert(type::from<rounding::floor>(-4.0).to<int>() == -4, "Bad floor rounding");
		static_assert(type::from<rounding::floor>(4.1).to<int>() == 4, "Bad floor rounding");
		static_assert(type::from<rounding::floor>(-4.1).to<int>() == -5, "Bad floor rounding");
		static_assert(type::from<rounding::floor>(4.5).to<int>() == 4, "Bad floor rounding");
		static_assert(type::from<rounding::floor>(-4.5).to<int>() == -5, "Bad floor rounding");
		static_assert(type::from<rounding::floor>(4.7).to<int>() == 4, "Bad floor rounding");
		static_assert(type::from<rounding::floor>(-4.7).to<int>() == -5, "Bad floor rounding");
	}

	// towards infinity
	{
		static_assert(type::from<rounding::infinity>(4.0).to<int>() == 4, "Bad towards-infinity rounding");
		static_assert(type::from<rounding::infinity>(-4.0).to<int>() == -4, "Bad towards-infinity rounding");
		static_assert(type::from<rounding::infinity>(4.1).to<int>() == 5, "Bad towards-infinity rounding");
		static_assert(type::from<rounding::infinity>(-4.1).to<int>() == -5, "Bad towards-infinity rounding");
		static_assert(type::from<rounding::infinity>(4.5).to<int>() == 5, "Bad towards-infinity rounding");
		static_assert(type::from<rounding::infinity>(-4.5).to<int>() == -5, "Bad towards-infinity rounding");
		static_assert(type::from<rounding::infinity>(4.7).to<int>() == 5, "Bad towards-infinity rounding");
		static_assert(type::from<rounding::infinity>(-4.7).to<int>() == -5, "Bad towards-infinity rounding");
	}

	// towards zero
	{
		static_assert(type::from<rounding::zero>(4.0).to<int>() == 4, "Bad towards-zero rounding");
		static_assert(type::from<rounding::zero>(-4.0).to<int>() == -4, "Bad towards-zero rounding");
		static_assert(type::from<rounding::zero>(4.1).to<int>() == 4, "Bad towards-zero rounding");
		static_assert(type::from<rounding::zero>(-4.1).to<int>() == -4, "Bad towards-zero rounding");
		static_assert(type::from<rounding::zero>(4.5).to<int>() == 4, "Bad towards-zero rounding");
		static_assert(type::from<rounding::zero>(-4.5).to<int>() == -4, "Bad towards-zero rounding");
		static_assert(type::from<rounding::zero>(4.7).to<int>() == 4, "Bad towards-zero rounding");
		static_assert(type::from<rounding::zero>(-4.7).to<int>() == -4, "Bad towards-zero rounding");
	}

	// nearest-up
	{
		static_assert(type::from<rounding::nearest_up>(4.0).to<int>() == 4, "Bad nearest-up rounding");
		static_assert(type::from<rounding::nearest_up>(-4.0).to<int>() == -4, "Bad nearest-up rounding");
		static_assert(type::from<rounding::nearest_up>(4.1).to<int>() == 4, "Bad nearest-up rounding");
		static_assert(type::from<rounding::nearest_up>(-4.1).to<int>() == -4, "Bad nearest-up rounding");
		static_assert(type::from<rounding::nearest_up>(4.5).to<int>() == 5, "Bad nearest-up rounding");
		static_assert(type::from<rounding::nearest_up>(-4.5).to<int>() == -4, "Bad nearest-up rounding");
		static_assert(type::from<rounding::nearest_up>(4.7).to<int>() == 5, "Bad nearest-up rounding");
		static_assert(type::from<rounding::nearest_up>(-4.7).to<int>() == -5, "Bad nearest-up rounding");
	}

	// nearest-down
	{
		static_assert(type::from<rounding::nearest_down>(4.0).to<int>() == 4, "Bad nearest-down rounding");
		static_assert(type::from<rounding::nearest_down>(-4.0).to<int>() == -4, "Bad nearest-down rounding");
		static_assert(type::from<rounding::nearest_down>(4.1).to<int>() == 4, "Bad nearest-down rounding");
		static_assert(type::from<rounding::nearest_down>(-4.1).to<int>() == -4, "Bad nearest-down rounding");
		static_assert(type::from<rounding::nearest_down>(4.5).to<int>() == 4, "Bad nearest-down rounding");
		static_assert(type::from<rounding::nearest_down>(-4.5).to<int>() == -5, "Bad nearest-down rounding");
		static_assert(type::from<rounding::nearest_down>(4.7).to<int>() == 5, "Bad nearest-down rounding");
		static_assert(type::from<rounding::nearest_down>(-4.7).to<int>() == -5, "Bad nearest-down rounding");
	}
}

void to_integral_rounding_test() {
	using namespace fix;
	using type = sfixed<4, 4>;

	// ceil
	{
		static_assert(type::from(3.5).to<int, rounding::ceil>() == 4, "Bad ceil rounding");
		static_assert(type::from(-3.5).to<int, rounding::ceil>() == -3, "Bad ceil rounding");
		static_assert(type::from(3.75).to<int, rounding::ceil>() == 4, "Bad ceil rounding");
		static_assert(type::from(-3.75).to<int, rounding::ceil>() == -3, "Bad ceil rounding");
		static_assert(type::from(3.25).to<int, rounding::ceil>() == 4, "Bad ceil rounding");
		static_assert(type::from(-3.25).to<int, rounding::ceil>() == -3, "Bad ceil rounding");
	}

	// floor
	{
		static_assert(type::from(3.5).to<int, rounding::floor>() == 3, "Bad floor rounding");
		static_assert(type::from(-3.5).to<int, rounding::floor>() == -4, "Bad floor rounding");
		static_assert(type::from(3.75).to<int, rounding::floor>() == 3, "Bad floor rounding");
		static_assert(type::from(-3.75).to<int, rounding::floor>() == -4, "Bad floor rounding");
		static_assert(type::from(3.25).to<int, rounding::floor>() == 3, "Bad floor rounding");
		static_assert(type::from(-3.25).to<int, rounding::floor>() == -4, "Bad floor rounding");
	}

	// towards-infinity
	{
		static_assert(type::from(3.5).to<int, rounding::infinity>() == 4, "Bad towards-infinity rounding");
		static_assert(type::from(-3.5).to<int, rounding::infinity>() == -4, "Bad towards-infinity rounding");
		static_assert(type::from(3.75).to<int, rounding::infinity>() == 4, "Bad towards-infinity rounding");
		static_assert(type::from(-3.75).to<int, rounding::infinity>() == -4, "Bad towards-infinity rounding");
		static_assert(type::from(3.25).to<int, rounding::infinity>() == 4, "Bad towards-infinity rounding");
		static_assert(type::from(-3.25).to<int, rounding::infinity>() == -4, "Bad towards-infinity rounding");
	}

	// towards-zero
	{
		static_assert(type::from(3.5).to<int, rounding::zero>() == 3, "Bad towards-zero rounding");
		static_assert(type::from(-3.5).to<int, rounding::zero>() == -3, "Bad towards-zero rounding");
		static_assert(type::from(3.75).to<int, rounding::zero>() == 3, "Bad towards-zero rounding");
		static_assert(type::from(-3.75).to<int, rounding::zero>() == -3, "Bad towards-zero rounding");
		static_assert(type::from(3.25).to<int, rounding::zero>() == 3, "Bad towards-zero rounding");
		static_assert(type::from(-3.25).to<int, rounding::zero>() == -3, "Bad towards-zero rounding");
	}

	// nearest-up
	{
		static_assert(type::from(3.5).to<int, rounding::nearest_up>() == 4, "Bad nearest up rounding");
		static_assert(type::from(-3.5).to<int, rounding::nearest_up>() == -3, "Bad nearest up rounding");
		static_assert(type::from(3.75).to<int, rounding::nearest_up>() == 4, "Bad nearest up rounding");
		static_assert(type::from(-3.75).to<int, rounding::nearest_up>() == -4, "Bad nearest up rounding");
		static_assert(type::from(3.25).to<int, rounding::nearest_up>() == 3, "Bad nearest up rounding");
		static_assert(type::from(-3.25).to<int, rounding::nearest_up>() == -3, "Bad nearest up rounding");
	}

	// nearest-down
	{
		static_assert(type::from(3.5).to<int, rounding::nearest_down>() == 3, "Bad nearest down rounding");
		static_assert(type::from(-3.5).to<int, rounding::nearest_down>() == -4, "Bad nearest down rounding");
		static_assert(type::from(3.75).to<int, rounding::nearest_down>() == 4, "Bad nearest down rounding");
		static_assert(type::from(-3.75).to<int, rounding::nearest_down>() == -4, "Bad nearest down rounding");
		static_assert(type::from(3.25).to<int, rounding::nearest_down>() == 3, "Bad nearest down rounding");
		static_assert(type::from(-3.25).to<int, rounding::nearest_down>() == -3, "Bad nearest down rounding");
	}

}

void scaling_shift_test()
{
	// Shifting up needs promotion
	constexpr fix::sfixed<31, 0> value_int32(1<<30);
	constexpr auto result = value_int32.scaling_shift<1>();
	static_assert(std::is_same<decltype(result.value), int>::value, "Foo");

	constexpr fix::sfixed<33, 0> value_int64(1ull << 32);
	constexpr auto result22 = value_int64.scaling_shift<-1>();

	constexpr auto val  = fix::ufixed<15, 0>::from(32767);
	constexpr auto conv = val.to<double>();
	
//	constexpr auto val = value.scaling_shift<1>();
}
/*
void div_test() {
	using nom = fix::sfixed<10, 12>;
	using den = fix::sfixed<2, 27>;
	
	constexpr auto some = fix::div<>(nom::from(-3.113), den::from(-0.0000001423));

	constexpr auto result = fix::div<fix::positive, fix::rounding::zero, fix::max_size<32>>(nom::from(-3.213), den::from(-0.001523));
	//static_assert(std::is_same<std::decay_t<decltype(result)>, fix::ufixed<3, 5>>::value, "Foooo!");
	constexpr auto value = result.to<double>();
}
*/

void div_test2() {
	using namespace fix;
	constexpr auto nom = integer_range<16000000000ull>(999982000ull);
	constexpr auto den = integer_range<16000000000ull>(1000051886ull);

	using nom_type = std::decay_t<decltype(nom)>;
	using den_type = std::decay_t<decltype(den)>;

	using div_diag = detail::div_struct< meta::list<fits<1, 31>, positive>, nom_type, den_type >;

	CA shift_nom = div_diag::shift_nom;
	CA shift_den = div_diag::shift_den;
	
	CA shifted_nom = nom.scaling_shift<shift_nom>();
	CA shifted_den = den.scaling_shift<shift_den>();

	CA result_f = div_diag::result_f;
	CA result_i = div_diag::result_i;
	
	constexpr auto result = div<fits<1,31>, positive>(nom, den);
	CA result_val = result.to<double>();

	CA scaling = ufixed<-28, 60>::scaling;
	constexpr auto seconds_factor = ufixed<-28, 60>::from(1e-9);
}


void add_test()
{
	using namespace fix;

	using a_type = fixed<28, -8, false, 1>;
	using b_type = fixed<12, -8, true, 15>;
	
	constexpr a_type lhs = a_type::from(272323);
	constexpr b_type rhs = b_type::from(1028);

	constexpr auto a_test = lhs.to<double>();
	constexpr auto b_test = rhs.to<double>();

	using add_sub_struct = detail::add_sub_struct< meta::list<fits<22, 0>>, a_type, b_type >;

	constexpr auto a_shift = add_sub_struct::a_shift;
	constexpr auto a_fixed = lhs.scaling_shift<a_shift>();
	constexpr auto a_value = a_fixed.to<double>();

	constexpr auto diff = add_sub_struct::exponent_difference;

	constexpr auto b_shift = add_sub_struct::b_shift;
	constexpr auto b_fixed = rhs.scaling_shift<b_shift>();
	constexpr auto b_value = b_fixed.to<double>();

	constexpr auto overshoot  = add_sub_struct::overshoot;
	constexpr auto undershoot = add_sub_struct::undershoot;

	constexpr auto sum = add<positive, fits<22,0>>(lhs, rhs);
	constexpr auto value = sum.to<double>();
}



void sub_test()
{
	using namespace fix;

	using a_type = decltype(integer(10000000000ull));
	using b_type = decltype(integer(10010000000ull));

	using div_diag = detail::div_struct< meta::list< fits<1, 31>, positive, max_size<64> >, a_type, b_type >;
	CA result_i = div_diag::result_i;
	CA result_f = div_diag::result_f;
	CA shift_a = div_diag::shift_nom;
	CA shift_b = div_diag::shift_den;
	
	constexpr auto drift = div<fits<1, 31>, positive, max_size<64>>(integer(10000000000ull), integer(10010000000ull));

}

void mul_test()
{
	using namespace fix;

	using a_type = sfixed<32, 0>;
	using b_type = sfixed<2, 30>;

	constexpr auto a = a_type::from(-89.214);
	constexpr auto a_value = a.to<double>();
	
	constexpr auto b = b_type::from(-1.4156);
	constexpr auto b_value = b.to<double>();
	
	using diag = detail::mul_struct<meta::list<fits<22,10>>, a_type, b_type>;

	constexpr auto overshoot = diag::overshoot;
	constexpr auto a_shift_temp = diag::a_shift_temp;
	constexpr auto b_shift_temp = diag::b_shift_temp;

	using shifted_a_type = diag::shifted_a_type;
	using shifted_b_type = diag::shifted_b_type;

	constexpr auto a_shift = diag::a_shift;
	constexpr auto b_shift = diag::b_shift;

	constexpr auto test = integer(32);

	//                                result > 0     9 int bits   restrict to 32 bit temporary (pre-mult rounding!)
	constexpr auto result = mul<fits<22,10>>(a, b);
	constexpr auto result_value = result.to<double>();
}

int main()
{

}
