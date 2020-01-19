# fixed

fixed point template metaprogramming library

Motivational example

``` c++
#include <fixed\fixed.hpp>
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

    // check result
    constexpr double sine = sinef.to<double>();
}
```

![Intellisense Screenshot](/../screenshots/screenshots/intellisense_sine_example.png?raw=true)

Most Basic usage

``` c++
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
```

All the types compile down to the fix::fixed template:

``` c++
    template< int I, int F, bool S, typename T, typename RT >
    struct fixed {
        // I:  integer bits (can be negative!)
        // F:  fraction bits (can be negative!)
        // S:  signed?
        // T:  smallest integral type capable of storing the fixed point number

        // RT: template instance of value_range< T, T min, T max >
        //     min and max hold the min and max values of this fixed point variable
        //     in fixed point notation
        //     they default to the mins and maxs possible within I, F, and S but if
        //     you provide custom bounds (FIXED_RANGE macro f.e.), they will constrain
        //     the value range further.
        //     All calculations done on the fixed point variables are carried out on
        //     these limits too, so the operations can try to act smart
    };

Supported rounding modes:

    rounding::floor
    rounding::ceil
    rounding::towards_zero
    rounding::towards_infinity
    rounding::nearest_up
    rounding::nearest_down
    // next_even, next_odd not implemented yet

Operations on fixed:

    // Shift number up (X > 0) or down (X < 0) and move I and F accordingly.
    auto shifted1 = scaling_shift<X [,rounding mode]>( fixed_point_var );

    // Shift I and F without touching the data (Ie. data interpretation scaled by 2^^X without losing precision)
    auto shifted2 = virtual_shift<X>( fixed_point_var );

    // Shift number up or down, but don't move I and F (Ie. scaled 2^^X)
    auto shifted2 = literal_shift<X [,rounding mode]>( fixed_point_var );

    add<[max_size, positive, fits<I [,F], rounding-mode]>( lhs, rhs );
    sub<[max_size, positive, fits<I [,F], rounding-mode]>( lhs, rhs );
    mul<[max_size, positive, fits<I [,F], rounding-mode]>( lhs, rhs );
    div<[max_size, positive, fits<I [,F], rounding-mode]>( lhs, rhs );

    // max_size restricts the size of intermediate results
    // fits<I [,F]> promises the result will fit into given typen
    // positive promises the result will be positive
    // rounding-mode sets the rounding-mode if rounding is necessary

    // operations generally try minimize precision loss, given the constraints
    // they're working in

    operator* (with result-size = max of input sizes, rounding to floor)
    operator+ (with result-size = max of input sizes, rounding to floor)
    operator- (with result-size = max of input sizes, rounding to floor)

    // no operator for div given, since any reasonable div operation relies
    // on smart constraints to be useful.
```

for further usage see fixed/fixed_test.cpp
