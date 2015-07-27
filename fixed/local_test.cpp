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

		using angle_type = FIXED_RANGE(-1.0, 1.0, 12);

		CA angle = angle_type::from(0.4);

		CA test = fix::virtual_shift<-1>(angle*(FIXED_CONSTANT_I(3)-fix::square(angle)));
		CA value = test.to<double>();
	}
}

#include <fixed\fixed.hpp>	
#define FCI(x) FIXED_CONSTANT_I(x)
#define FCR(x,y) FIXED_CONSTANT(x, y)

template< typename T, typename S = fix::enable_if_fixed_t<T> >
constexpr auto sine_5th_order(T angle)
{
	using namespace fix;
	constexpr int precision = T::data_bits;

	// 3 constants for 5th order sine taylor approx
	constexpr double pi = 3.1415926535897932384626433832795;
	constexpr auto a = FCR(4.0*(3.0 / pi - 9.0 / 16.0), precision);
	constexpr auto b = vshift<1>(a) - FCR(2.5, precision);
	constexpr auto c = a - FCR(1.5, precision);

	// calculate sine
	return (angle*(a - square(angle)*(b - square(angle)*c)));
}

void test_sine()
{
	constexpr auto angle = FIXED_RANGE(-1.0, 1.0, 16)::from(0.5);
	constexpr auto sinef = sine_5th_order(angle);

	// check result
	constexpr double sine = sinef.to<double>();
}