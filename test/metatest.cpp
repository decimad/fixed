//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <fixed/fixed.hpp>
#include <fixed/fixedmeta.hpp>


template <int a, int b>
struct some_template
{
};

void meta_test()
{
    using namespace fix;

    constexpr auto val = util::mixed_min(-4.4, 15123553);
}
