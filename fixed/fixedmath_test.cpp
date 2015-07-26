//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <fixed/fixedmath.hpp>

namespace {

	using namespace fix;
	using namespace fix::util;
	
	void log2_test()
	{
		static_assert(log2_ceil(1) == 0, "log2_ceil<integral> broken.");
		static_assert(log2_ceil(2) == 1, "log2_ceil<integral> broken.");
		
		static_assert(log2_ceil(32767.0) == 15, "log2_ceil<floating> broken.");
		static_assert(log2_ceil(32768.0) == 15, "log2_ceil<floating> broken.");

		static_assert(log2_ceil(2.1) == 2, "log2_ceil<floating> broken.");
		static_assert(log2_ceil(2.0) == 1, "log2_ceil<floating> broken.");
		static_assert(log2_ceil(1.9) == 1, "log2_ceil<floating> broken.");
		static_assert(log2_ceil(1.0) == 0, "log2_ceil<floating> broken.");
		static_assert(log2_ceil(0.9) == 0, "log2_ceil<floating> broken.");
		
		static_assert(log2_ceil(0.5)   == -1, "log2_ceil<floating> broken.");
		static_assert(log2_ceil(0.25)  == -2, "log2_ceil<floating> broken.");
		static_assert(log2_ceil(0.125) == -3, "log2_ceil<floating> broken.");
		
		static_assert(log2_floor(2.1) ==  1, "log2_floor<floating> broken.");
		static_assert(log2_floor(2.0) ==  1, "log2_floor<floating> broken.");
		static_assert(log2_floor(1.9) ==  0, "log2_floor<floating> broken.");
		static_assert(log2_floor(1.0) ==  0, "log2_floor<floating> broken.");
		static_assert(log2_floor(0.9) == -1, "log2_floor<floating> broken.");

		static_assert(log2_floor(0.5)   == -1, "log2_floor<floating> broken.");
		static_assert(log2_floor(0.25)  == -2, "log2_floor<floating> broken.");
		static_assert(log2_floor(0.125) == -3, "log2_floor<floating> broken.");
		
		static_assert(log2_ceil(1)  == log2_ceil(1.0),  "log2_ceil broken.");
		static_assert(log2_ceil(2)  == log2_ceil(2.0),  "log2_ceil broken.");
		static_assert(log2_ceil(4)  == log2_ceil(4.0),  "log2_ceil broken.");
		static_assert(log2_ceil(8)  == log2_ceil(8.0),  "log2_ceil broken.");
		static_assert(log2_ceil(16) == log2_ceil(16.0), "log2_ceil broken.");
		
		// static_assert(log2_ceil(0) == log2_ceil(0.0), "log2_ceil broken.");	// must not compile
	}

	void abs2_test()
	{
		static_assert(abs2(short(-32768)) == 32768, "abs2 broken.");
		static_assert(abs2(char(-128)) == 128, "abs2 broken.");
	}

	void integer_bits_float_test()
	{
		// (0...1)
		static_assert(integer_bits(0.25) == -1, "integer_bits<float> broken.");		
		static_assert(integer_bits(0.26) == -1, "integer_bits<float> broken.");
		static_assert(integer_bits(0.49) == -1, "integer_bits<float> broken.");
		static_assert(integer_bits(0.5) == 0, "integer_bits<float> broken.");
		static_assert(integer_bits(0.9) == 0, "integer_bits<float> broken.");
		
		// (1...oo)
		static_assert(integer_bits(1.0) == 1, "integer_bits<float> broken.");
		static_assert(integer_bits(2.0) == 2, "integer_bits<float> broken.");
		static_assert(integer_bits(4.0) == 3, "integer_bits<float> broken.");
		static_assert(integer_bits(8.0) == 4, "integer_bits<float> broken.");

		// (-1...0)
		static_assert(integer_bits(-0.26) == 0, "integer_bits<float> broken.");
		static_assert(integer_bits(-0.25) == 0, "integer_bits<float> broken.");
		static_assert(integer_bits(-0.24) == -1, "integer_bits<float> broken.");

		// (-oo...1)
		static_assert(integer_bits(-1.0) == 1, "integer_bits<float> broken.");
		static_assert(integer_bits(-2.0) == 2, "integer_bits<float> broken.");
		static_assert(integer_bits(-3.0) == 3, "integer_bits<float> broken.");
		static_assert(integer_bits(-4.0) == 3, "integer_bits<float> broken.");
	}

	void integer_bits_test()
	{
		static_assert(integer_bits(-1) == 1, "integer_bits<integral> broken.");
		static_assert(integer_bits(0) == 1, "integer_bits<integral> broken.");
		static_assert(integer_bits(1) == 1, "integer_bits<integral> broken.");
		static_assert(integer_bits(2) == 2, "integer_bits<integral> broken.");
		
		static_assert(integer_bits(65537) == 17, "integer_bits<integral> broken.");
		static_assert(integer_bits(65536) == 17, "integer_bits<integral> broken.");
		static_assert(integer_bits(65535) == 16, "integer_bits<integral> broken.");

		static_assert(integer_bits(32768)  == 16, "integer_bits<integral> broken.");
		static_assert(integer_bits(32767)  == 15, "integer_bits<integral> broken.");
		static_assert(integer_bits(32766)  == 15, "integer_bits<integral> broken.");

		static_assert(integer_bits(-32767) == 16, "integer_bits<integral> broken.");
		static_assert(integer_bits(-32768) == 16, "integer_bits<integral> broken.");
		static_assert(integer_bits(-32769) == 17, "integer_bits<integral> broken.");

		// check if max values for given type overflow
		static_assert(integer_bits(unsigned short(65535)) == 16, "integer_bits<integral> broken.");
	}

	void range_bits_test()
	{
		static_assert(range_bits(uint8(0), uint8(1)) == 1, "range_bits<integral, integral> broken");
		static_assert(range_bits(-1, 0)         == 1, "range_bits<integral, integral> broken.");
		static_assert(range_bits(-2, 1)         == 2, "range_bits<integral, integral> broken.");
		static_assert(range_bits(-128, 127)     == 8, "range_bits<integral, integral> broken.");
		static_assert(range_bits(-32768, 32767) == 16, "range_bits<integral, integral> broken.");
		
		constexpr auto val  = integer_bits(128);
		constexpr auto val2 = integer_bits(1.0);

		static_assert(range_bits(-1.0, 1.0)     == 2, "range_bits<integral, integral> broken.");
		static_assert(range_bits(-1.0, 0.9)     == 1, "range_bits<integral, integral> broken.");

		static_assert(range_bits(0.99, 1.01) == 1, "range_bits<float, float> broken.");
	}

}