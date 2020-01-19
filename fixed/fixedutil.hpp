//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FIXED_FIXEDUTIL_HPP__
#define FIXED_FIXEDUTIL_HPP__

#include <fixed/fixedtypes.hpp>
#include <type_traits>

namespace fix::util
{

    template <bool Cond, typename A, typename B>
    using conditional_t = typename std::conditional<Cond, A, B>::type;

    template <typename T>
    using make_unsigned_t = typename std::make_unsigned<T>::type;

    template <typename T>
    using make_signed_t = typename std::make_signed<T>::type;

    template <bool Cond, typename T = void>
    using enable_if_t = typename std::enable_if<Cond, T>::type;

    template <typename T, typename S = void>
    using enable_if_integral_t = enable_if_t<std::is_integral<T>::value, S>;

    template <typename T, typename S = void>
    using enable_if_floating_point_t = enable_if_t<std::is_floating_point<T>::value, S>;

    template <typename T1, typename T2>
    struct larger_type
    {
        using type = conditional_t<(sizeof(T1) > sizeof(T2)), T1, T2>;
    };

    template <typename T1, typename T2>
    using larger_type_t = typename larger_type<T1, T2>::type;

    template <typename T, typename S>
    constexpr bool safe_less(T t, S s)
    {
        using lt = larger_type_t<T, S>;
        using st = make_signed_t<lt>;
        using ut = make_unsigned_t<lt>;

        // For matching signedness case we don't need to cast really (and differ between signed&unsigned), but MSVC issues a warning.
        return (std::is_signed<T>::value && std::is_signed<S>::value)
                   ? (st(t) < st(s))
                   : ((std::is_unsigned<T>::value && std::is_unsigned<S>::value)
                          ? (ut(t) < ut(s))
                          : ((std::is_signed<T>::value && t < 0) ? (true)
                                                                 : ((std::is_signed<S>::value && s < 0) ? (false)
                                                                                                        : (
                                                                                                            // both fit into ut
                                                                                                            ut(t) < ut(s)))));
    }

    template <typename T>
    constexpr T max(T lhs, T rhs)
    {
        return (lhs > rhs) ? lhs : rhs;
    }

    template <typename T, typename... Args>
    constexpr T max_n(T t1, T t2, T t3, Args &&... t)
    {
        return max(max(t1, t2), t3, t...);
    }

    template <typename T>
    constexpr T min(T lhs, T rhs)
    {
        return (lhs > rhs) ? rhs : lhs;
    }

    template <typename T, typename... Args>
    constexpr T min_n(T t1, T t2, T t3, Args... t)
    {
        return min(min(t1, t2), t3, t...);
    }

    template <typename T, typename S>
    constexpr S saturate(T t, S s1, S s2)
    {
        return safe_less(t, util::min(s1, s2)) ? util::min(s1, s2) : (safe_less(util::max(s1, s2), t) ? util::max(s1, s2) : S(t));
    }

    template <typename T>
    struct promoted_type
    {

        using intermediate_type = conditional_t<sizeof(T) == 1, short, conditional_t<sizeof(T) == 2, int, int64>>;

        using type = conditional_t<std::is_signed<T>::value, make_signed_t<intermediate_type>, make_unsigned_t<intermediate_type>>;
    };

    template <typename T>
    using promoted_type_t = typename promoted_type<T>::type;

    template <typename T, bool Value = std::is_integral<T>::value>
    struct make_signed_if_integral
    {
        using type = T;
    };

    template <typename T>
    struct make_signed_if_integral<T, true>
    {
        using type = make_signed_t<T>;
    };

    template <typename T>
    using make_signed_if_integral_t = typename make_signed_if_integral<T>::type;

    template <typename T, bool Value = std::is_integral<T>::value>
    struct make_unsigned_if_integral
    {
        using type = T;
    };

    template <typename T>
    struct make_unsigned_if_integral<T, true>
    {
        using type = make_unsigned_t<T>;
    };

    template <typename T>
    using make_unsigned_if_integral_t = typename make_unsigned_if_integral<T>::type;

    namespace detail
    {

        template <typename A, typename B>
        struct fitting_integral_type
        {

            using type
                = conditional_t<std::is_signed<A>::value == std::is_signed<B>::value, conditional_t<(sizeof(A) > sizeof(B)), A, B>,
                                conditional_t<sizeof(A) == sizeof(B), make_signed_if_integral_t<promoted_type_t<larger_type_t<A, B>>>,
                                              make_signed_if_integral_t<larger_type_t<A, B>>>>;
        };

        template <typename A, typename B>
        using fitting_integral_type_t = typename fitting_integral_type<A, B>::type;

        template <typename A, typename B>
        struct mixed_type
        {

            using type = conditional_t<std::is_floating_point<A>::value && std::is_floating_point<B>::value,
                                       conditional_t<(sizeof(A) > sizeof(B)), A, B>,
                                       conditional_t<std::is_floating_point<A>::value, A,
                                                     conditional_t<std::is_floating_point<B>::value, B, fitting_integral_type_t<A, B>>>>;
        };

        template <typename A, typename B>
        using mixed_type_t = typename mixed_type<A, B>::type;

    } // namespace detail

    template <typename A, typename B>
    constexpr detail::mixed_type_t<A, B> mixed_min(A a, B b)
    {
        using type = detail::mixed_type_t<A, B>;
        return min(type(a), type(b));
    }

    template <typename A, typename B>
    constexpr detail::mixed_type_t<A, B> mixed_max(A a, B b)
    {
        using type = detail::mixed_type_t<A, B>;
        return max(type(a), type(b));
    }

} // namespace fix::util

#endif
