//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)


#include "fixedmeta.hpp"

template< int a, int b >
struct some_template {};

void meta_test()
{
	using list = meta::list< some_template<1, 2> >;
	using T = meta::value_template<int, int>::finder<some_template>;

	//using result_type = meta::find_if< list, meta::value_template<int, int>::finder<some_template> >::type;
	//static_assert(std::is_same<result_type, some_template<1, 2>>::value, "Fooo");
}