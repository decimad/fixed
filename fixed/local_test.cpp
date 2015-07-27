#include "fixed.hpp"

namespace {

#define CA constexpr auto


	void microsoft_test() {

		typedef fix::value_range <short, -2046, 2047> foo_type;
		typedef fix::value_range2<short, -2047, 2047> foo_type2;
		
		constexpr auto bai = foo_type ::bits;
		constexpr auto fov = foo_type2::bits;

	}

	void test424()
	{
		using fix::sfixed;
		using fix::virtual_shift;
		using fix::ufixed;
		using fix::value_range;

		std::numeric_limits<int>::max();

		CA angle = fix::virtual_shift<-11>(sfixed<12, 0, value_range<short, -2047, 2047>>::from(-2047));
		CA angle2 = fix::virtual_shift<-11>(sfixed<12, 0, value_range<short, -2047, 2047>>::from(-2047));

		CA test = fix::mul<fix::positive>(angle, angle);
		CA test2 = angle * angle2;
		constexpr auto test3 = angle + angle2;
		CA test4 = angle - angle2;


		CA product = fix::square(fix::square(angle));
		CA value = product.to<double>();
	}
	

}

