//          Copyright Michael Steinberg 2015
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FIXED_FIXEDMETA_HPP__
#define FIXED_FIXEDMETA_HPP__

#include <type_traits>
#include <fixed/fixedutil.hpp>

namespace fix {

	namespace  meta {

		template< typename... Types >
		struct list {};

		template< typename List, typename Functor>
		struct find_if;

		template< typename List, typename Functor >
		using find_if_t = typename find_if<List, Functor>::type;

		struct void_type {};

		template< typename List, typename T >
		struct contains;

		template< typename Elem0, typename... Elems, typename T >
		struct contains< list< Elem0, Elems... >, T >
		{
			static constexpr bool value = std::is_same<T, Elem0>::value ? true : contains< list<Elems...>, T >::value;
		};

		template< typename T >
		struct contains< list< >, T >
		{
			static constexpr bool value = false;
		};

		template< typename T0, typename... Types, typename Functor>
		struct find_if< list<T0, Types...>, Functor > {
			using applied = typename Functor::template apply<T0>;
			using type = util::conditional_t<applied::value, typename applied::type, find_if_t< list<Types...>, Functor >>;
		};

		template< typename Functor >
		struct find_if< list<>, Functor >
		{
			using type = void_type;
		};

		template< typename List, typename Functor, typename Or >
		struct find_if_or
		{
			using intermediate = find_if_t< List, Functor >;
			using type = util::conditional_t<std::is_same<intermediate, void_type>::value, Or, intermediate >;
		};

		template< typename List, typename Functor, typename Or >
		using find_if_or_t = typename find_if_or<List, Functor, Or>::type;

		template< typename T >
		struct integral_constant_finder
		{
			template< typename Elem >
			struct apply {
				using type = void_type;
				static constexpr bool value = false;
			};

			template<T Value>
			struct apply< std::integral_constant<T, Value> >
			{
				using type = std::integral_constant<T, Value>;
				static constexpr bool value = true;
			};
		};

		struct true_type {
			static constexpr bool value = true;
		};

		struct false_type {
			static constexpr bool value = false;
			using type = void_type;
		};

	}

}

#endif
