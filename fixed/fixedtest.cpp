//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <fixed/fixed.hpp>

void storage_type_test() {
	constexpr fix::sfixed<31, 0> value_int32(0);
	static_assert(std::is_same< decltype(value_int32)::value_type, int >::value, "Wrong storage type");

	constexpr fix::ufixed<16, 0> value_uint16(0);
	static_assert(std::is_same< decltype(value_uint16)::value_type, unsigned short >::value, "Wrong storage type");
}

void macro_test()
{
	constexpr auto size_value = FIXED_VALUE_S(2.45, 32);	// Given result size
	constexpr auto prec_value = FIXED_VALUE_P(2.45, 8);		// Given result precision

	using range_p_value_type = FIXED_RANGE_TYPE_P(-2.45, 10.45, 5);
	using range_s_value_type = FIXED_RANGE_TYPE_S(-2.45, 10.45, 32);

	constexpr auto conv1 = size_value.to<double>();
	constexpr auto conv2 = prec_value.to<double>();
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

void div_test() {
	using nom = fix::sfixed<10, 12>;
	using den = fix::sfixed<2, 27>;
	
	constexpr auto some = fix::div<>(nom::from(-3.113), den::from(-0.0000001423));

	constexpr auto result = fix::div</*fix::fits<3,5>,*/ fix::positive, fix::rounding::zero, fix::max_size<32>>(nom::from(-3.213), den::from(-0.001523));
	//static_assert(std::is_same<std::decay_t<decltype(result)>, fix::ufixed<3, 5>>::value, "Foooo!");
	constexpr auto value = result.to<double>();
}


void add_test()
{
	using a_type = fix::fixed<28, -8, false, 1>;
	using b_type = fix::fixed<12, -8, true, 15>;
	
	constexpr a_type lhs = a_type::from(272323);
	constexpr b_type rhs = b_type::from(1028);

	constexpr auto a_test = lhs.to<double>();
	constexpr auto b_test = rhs.to<double>();

	using add_sub_struct = fix::detail::add_sub_struct< fix::meta::list<fix::fits<22, 0>>, a_type, b_type >;

	constexpr auto a_shift = add_sub_struct::a_shift;
	constexpr auto a_fixed = lhs.scaling_shift<a_shift>();
	constexpr auto a_value = a_fixed.to<double>();

	constexpr auto diff = add_sub_struct::exponent_difference;

	constexpr auto b_shift = add_sub_struct::b_shift;
	constexpr auto b_fixed = rhs.scaling_shift<b_shift>();
	constexpr auto b_value = b_fixed.to<double>();

	constexpr auto overshoot  = add_sub_struct::overshoot;
	constexpr auto undershoot = add_sub_struct::undershoot;

	constexpr auto sum = fix::add<fix::positive, fix::fits<22,0>>(lhs, rhs);
	constexpr auto value = sum.to<double>();
}

void mul_test()
{
	using a_type = fix::sfixed<12, 16>;
	using b_type = fix::sfixed<8, 16>;

	constexpr auto a = a_type::from(-89.214);
	constexpr auto a_value = a.to<double>();
	
	constexpr auto b = b_type::from(-5.4156);
	constexpr auto b_value = b.to<double>();
	
	//                                result > 0     9 int bits   restrict to 32 bit temporary (pre-mult rounding!)
	constexpr auto result = fix::mul<fix::positive, fix::fits<9>, fix::max_size<32>>(a, b);
	constexpr auto result_value = result.to<double>();
}
