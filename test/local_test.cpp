#include <fixed/fixed.hpp>

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

#include <fixed/fixed.hpp>	
#define FCI(x) FIXED_CONSTANT_I(x)
#define FCR(x,y) FIXED_CONSTANT(x, y)

template<int Precision>
struct sine_5th_constants {
	// 3 constants for 5th order sine taylor approx
	static constexpr double pi = 3.1415926535897932384626433832795;
	static constexpr auto a = FCR(4.0*(3.0 / pi - 9.0 / 16.0), Precision);
	static constexpr auto b = fix::vshift<1>(a) - FCR(2.5, Precision);
	static constexpr auto c = a - FCR(1.5, Precision);
};

template< typename T, typename S = fix::enable_if_fixed_t<T> >
constexpr auto sine_5th_order(T angle)
{
	using namespace fix;
	using cons = sine_5th_constants< T::data_bits >;
	
	// calculate sine
	return (angle*(cons::a - square(angle)*(cons::b - square(angle)*cons::c)));
}

void test_sine()
{
	constexpr auto angle = FIXED_RANGE(-1.0, 1.0, 16)::from(0.5);
	constexpr auto sinef = sine_5th_order(angle);

	constexpr int bits = sinef.data_bits;
	constexpr int integer = sinef.integer_bits;
	constexpr int fractio = sinef.fractional_bits;

	constexpr auto maxval_f = sinef.max().value;
	constexpr auto minval_f = sinef.min().value;

	constexpr double maxval = sinef.max().to<double>();
	constexpr double minval = sinef.min().to<double>();

	// check result
	constexpr double sine = sinef.to<double>();
}
