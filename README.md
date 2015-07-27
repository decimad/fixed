# fixed
fixed point template metaprogramming library

Most Basic usage

    #include "fixed.hpp"
    using namespace fix;
    
    ufixed<3,3> foo; // 3.3 unsigned fixedpoint
    sfixed<3,3> bar; // 3.3 signed fixedpoint
    
    // -13.45 (32 bit) unsigned
    using foo_type = FIXED_RANGE(0, 0.0001, 32); 
    
    // all conversions constexpr
    constexpr foo_type my_val = foo_type::from(0.00004);
    constexpr auto test = my_val.to<double>()
    
    // -12.44 (32 bit) signed
    using bar_type = FIXED_RANGE(-0.0001, 0.0001, 32) bar2;
    
    // Constants
    constexpr auto some_float_constant   = FIXED_CONSTANT(44.5632, 32);
    constexpr auto some_integer_constant = FIXED_CONSTANT_I(563);
    
    // automatic sizing for integer ranges
    FIXED_RANGE_I(-232, 5342) some;


for usage see fixed/fixedtest.cpp
