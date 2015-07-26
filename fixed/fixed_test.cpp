//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <fixed/fixed.hpp>

#define CA constexpr auto

namespace {

	void range_mul_test()
	{
		using namespace fix;
		CA angle = virtual_shift<-11>(sfixed<12, 0, value_range<int16, -2047, 2047>>::from(-2047));
		
		using test_type = DEBUG_ADD_SUB(angle, angle);

		CA fgas = util::range_bits(short(-2047), short(2047));

		using foo_type = value_range<int16, -2047, 2047>; //std::decay_t<decltype(angle)>::range_type;
		constexpr auto bits64 = util::range_bits(short(-2047), short(2047));
		constexpr auto bits32 = foo_type::bits;

		using result_type = detail::add_result_type_t<std::decay_t<decltype(angle)>::range_type, std::decay_t<decltype(angle)>::range_type>;
		CA maxval = detail::max_add_result< std::decay_t<decltype(angle)>::range_type, std::decay_t<decltype(angle)>::range_type >::value;
		CA minval = detail::min_add_result< std::decay_t<decltype(angle)>::range_type, std::decay_t<decltype(angle)>::range_type >::value;

		using my_range = value_range<result_type, maxval, minval>;
		CA fok = my_range::is_signed;
		CA fuk = my_range::bits;
		CA fek = my_range::max;
		CA fik = my_range::min;


		using test_type2 = detail::mul_result_range_t<std::decay_t<decltype(angle)>::range_type, std::decay_t<decltype(angle)>::range_type>;
		CA bits = test_type2::bits;

		CA test = mul<positive>(angle, angle);
		CA test2 = angle * angle;
		CA test3 = angle + angle;


		CA product = fix::square(fix::square(angle));
		CA value = product.to<double>();
	}

	void auto_range_test()
	{
		using namespace fix;

		static_assert(detail::auto_fixed_range<1, false>::bits == 1, "Foo!");
		static_assert(detail::auto_fixed_range< 2, true>::bits == 2, "Foo!");
		static_assert(detail::auto_fixed_range< 3, true>::bits == 3, "Foo!");
		static_assert(detail::auto_fixed_range< 8, true>::bits == 8, "Foo!");
		static_assert(detail::auto_fixed_range<16, true>::bits == 16, "Foo!");
		static_assert(detail::auto_fixed_range<32, true>::bits == 32, "Foo!");

		using range_type = RANGE_FROM_VALS(-3232, 5136);
		//bits = range_type::bits;
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

		constexpr auto size_value = FIXED_CONSTANT(2.45, 32);	// Given result size
		constexpr auto prec_value = FIXED_CONSTANT_P(2.45, 8);	// Given result precision

		using range_p_value_type = FIXED_RANGE_P(-2.45, 10.45, 5);

		CA bits3 = util::integer_bits(-32768);

		CA bits = util::range_bits(-2.45, 10.45);
		CA bits2 = util::range_bits(1231, -32768);

		CA isneg = ::fix::util::any_neg(-2.45, 10.45);

		using range_type = ::fix::value_range<::fix::detail::value_type_t<(bits)+(32 - bits), ::fix::util::any_neg(-2.45, 10.45)>, ::fix::detail::to_fixed<bits, 32 - bits, ::fix::util::any_neg(-2.45, 10.45)>(-2.45), ::fix::detail::to_fixed<bits, 32 - bits, ::fix::util::any_neg(-2.45, 10.45)>(10.45)>::min_range_type;

		using range_s_value_type = FIXED_RANGE(-2.45, 10.45, 32);

		constexpr auto conv1 = size_value.to<double>();
		constexpr auto conv2 = prec_value.to<double>();
	}

	void assign_test()
	{
		using namespace fix;

		using type_a = FIXED_RANGE_I(0, 255);
		using type_b = FIXED_RANGE_I(-16000, 16000);

		type_a value;
		value = type_b(15000);
		//value.assign_saturated(type_b(15000));

		CA val4 = util::range_bits(0, 0.0001);

		CA val7 = util::abs(-4.0);
		CA val8 = util::log2_floor(0.1);
		CA val9 = util::log2_ceil(0.1);
		CA val95 = util::log2_floor(val8);

		CA val6 = util::log2_ceil(4.1);
		CA val354 = util::log2_floor(4.1);

		CA val634 = util::integer_bits(-4);
		CA val563 = util::integer_bits(-4.0);


		CA val5 = util::range_bits(0, -8);

		CA val = UINT_MAX;
		CA fasf = util::scaled_exp2<util::RoundModes::Floor>(0.0001, 45);
		CA fasf2 = util::integer_bits(-0.0001);

		CA val44 = detail::to_fixed<-13, 45, false>(0.0001);

		using my_type = FIXED_RANGE(0, 0.0001, 32);
		CA maxval = my_type::max().to<double>();
		CA minval = my_type::min().to<double>();

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
		using namespace fix;

		// Shifting up needs promotion
		constexpr sfixed<31, 0> value_int32(1 << 30);
		constexpr auto result = scaling_shift<1>(value_int32);
		static_assert(std::is_same<decltype(result.value), int>::value, "Foo");

		constexpr sfixed<33, 0> value_int64(1ull << 32);
		constexpr auto result22 = scaling_shift<-1>(value_int64);

		constexpr auto val = fix::ufixed<15, 0>::from(32767);
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

		CA shifted_nom = scaling_shift<shift_nom>(nom);
		CA shifted_den = scaling_shift<shift_den>(den);

		CA result_f = div_diag::result_f;
		CA result_i = div_diag::result_i;

		constexpr auto result = div<fits<1, 31>, positive>(nom, den);
		CA result_val = result.to<double>();

		CA scaling = ufixed<-28, 60>::scaling;
		constexpr auto seconds_factor = ufixed<-28, 60>::from(1e-9);
	}

	void add_test()
	{
		using namespace fix;

		using a_type = fixed<28, -8, false>;	// 20 bits
		using b_type = fixed<12, -8, true >;	//  4 bits

		constexpr a_type lhs = a_type::from(272323);
		constexpr b_type rhs = b_type::from(-1022);

		constexpr auto a_test = lhs.to<double>();
		constexpr auto b_test = rhs.to<double>();

		using add_sub_struct = detail::add_sub_struct< meta::list<positive, fits<22, 0>>, a_type, b_type >;

		constexpr auto a_shift = add_sub_struct::a_shift;
		constexpr auto a_fixed = scaling_shift<a_shift>(lhs);
		constexpr auto a_value = a_fixed.to<double>();

		constexpr auto diff = add_sub_struct::exponent_difference;

		constexpr auto b_shift = add_sub_struct::b_shift;
		constexpr auto b_fixed = scaling_shift<b_shift>(rhs);
		constexpr auto b_value = b_fixed.to<double>();

		constexpr auto test2 = scaling_shift<b_shift>(rhs);
		constexpr auto test3 = virtual_shift<-1>(rhs);
		constexpr auto test4 = test3.to<double>();

		constexpr auto test5 = literal_shift<0>(rhs);
		constexpr auto test6 = test5.to<double>();


		constexpr auto bexp0 = b_fixed.exponent(0);
		constexpr auto aexp0 = a_fixed.exponent(0);

		constexpr auto overshoot = add_sub_struct::overshoot;
		//	constexpr auto undershoot = add_sub_struct::undershoot;

		using add_result = add_sub_struct::add_result_type;

		constexpr auto fum = add<positive, fits<22, 0>>(lhs, rhs);
		constexpr auto value = fum.to<double>();
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

		using diag = detail::mul_struct<meta::list<fits<22, 10>>, a_type, b_type>;

		constexpr auto overshoot = diag::overshoot;
		constexpr auto a_shift_temp = diag::a_shift_temp;
		constexpr auto b_shift_temp = diag::b_shift_temp;

		using shifted_a_type = diag::shifted_a_type;
		using shifted_b_type = diag::shifted_b_type;

		constexpr auto a_shift = diag::a_shift;
		constexpr auto b_shift = diag::b_shift;

		constexpr auto test = integer(32);

		//                                result > 0     9 int bits   restrict to 32 bit temporary (pre-mult rounding!)
		constexpr auto result = mul<fits<22, 10>>(a, b);
		constexpr auto result_value = result.to<double>();
	}

}
