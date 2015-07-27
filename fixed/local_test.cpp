#include "fixed.hpp"

namespace {

#define CA constexpr auto

	void test()
	{
		using fix::sfixed;
		using fix::virtual_shift;
		using fix::ufixed;
		using fix::value_range;

		CA angle  = fix::virtual_shift<-11>(sfixed<12, 0, value_range<short, -2047, 2047>>::from(-2047));
		CA angle2 = fix::virtual_shift<-11>(sfixed<12, 0, value_range<short, -2047, 2047>>::from(-2047));

		using angle_type = std::decay_t<decltype(angle)>;
		using angle_range_type = angle_type::range_type;
		using angle_range_value_type = angle_range_type::value_type;

		using sub_result_value_type = fix::detail::sub_result_value_type_t<angle_range_type, angle_range_type>;
		CA maxval2 = fix::detail::max_sub_result<angle_range_type, angle_range_type>::value;


		static_assert(std::is_same<sub_result_value_type, short>::value, "Fooo!");

		using sub_struct_type = fix::detail::add_sub_struct <fix::meta::list<>, angle_type, angle_type>;
		
		CA val = std::is_same<angle_range_value_type, long long>::value;

		using test_type = DEBUG_ADD_SUB(angle, angle);

		CA fgas = fix::util::range_bits(short(-2047), short(2047));

		using foo_type = value_range<short, -2047, 2047>;
		constexpr auto bits64 = fix::util::range_bits(short(-2047), short(2047));
		constexpr auto bits32 = foo_type::bits;

		using result_type = fix::detail::add_result_value_type_t<std::decay_t<decltype(angle)>::range_type, std::decay_t<decltype(angle)>::range_type>;
		
		CA maxval = fix::detail::max_add_result< std::decay_t<decltype(angle)>::range_type, std::decay_t<decltype(angle)>::range_type >::value;
		CA minval = fix::detail::min_add_result< std::decay_t<decltype(angle)>::range_type, std::decay_t<decltype(angle)>::range_type >::value;

		using my_range = value_range<result_type, maxval, minval>;

		CA fok = my_range::is_signed;
		CA fuk = my_range::bits;
		CA fek = my_range::maxval;
		CA fik = my_range::minval;

		using test_type2 = fix::detail::mul_result_range_t<std::decay_t<decltype(angle)>::range_type, std::decay_t<decltype(angle)>::range_type>;
		CA bits = test_type2::bits;

		CA test = fix::mul<fix::positive>(angle, angle);
		CA test2 = angle * angle2;
		constexpr auto test3 = angle + angle2;
		CA test4 = angle - angle2;


		CA product = fix::square(fix::square(angle));
		CA value = product.to<double>();
	}


}

int main()
{

}