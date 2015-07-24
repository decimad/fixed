#ifndef FIXED_FIXEDUTIL_HPP__
#define FIXED_FIXEDUTIL_HPP__

#include <type_traits>

namespace fix { namespace util {
	
	template<typename T>
	constexpr T max(T lhs, T rhs)
	{
		return (lhs > rhs) ? lhs : rhs;
	}

	template<typename T, typename... Args>
	constexpr T max(T t1, T t2, T t3, Args&&... t)
	{
		return max(max(t1, t2), t3, t...);
	}

	template<typename T>
	constexpr T min(T lhs, T rhs)
	{
		return (lhs > rhs) ? rhs : lhs;
	}

	template<typename T, typename... Args>
	constexpr T min(T t1, T t2, T t3, Args... t)
	{
		return min(min(t1, t2), t3, t...);
	}
	
	namespace detail {

		template< typename T1, typename T2 >
		struct larger_type {
			using type = typename std::conditional<(sizeof(T1)>sizeof(T2)), T1, T2 > ::type;
		};

		template<typename T1, typename T2>
		using larger_type_t = typename larger_type<T1, T2>::type;

	}

	template< typename T, typename S >
	constexpr bool safe_less(T t, S s)
	{
		using lt = detail::larger_type_t<T, S>;
		using ut = typename std::make_unsigned<lt>::type;

		return
			(std::is_signed<T>::value == std::is_signed<S>::value) ? (lt(t) < lt(s)) : (
				(std::is_signed<T>::value && t < 0) ? (true) : (
					(std::is_signed<S>::value && s < 0) ? (false) : (
						// both fit in an unsigned type
						ut(t) < ut(s)
						)
					)
				)
			;
	}

	template< typename T, typename S >
	constexpr S saturate(T t, S s1, S s2)
	{
		return safe_less(t, util::min(s1, s2)) ? util::min(s1, s2) : (safe_less(util::max(s1, s2), t) ? util::max(s1, s2) : S(t));
	}

} }

#endif