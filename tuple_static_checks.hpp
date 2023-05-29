#include "tuple.hpp"

namespace tuple_static_checks
{
	struct non_trivial
	{
		constexpr non_trivial() {}
		constexpr non_trivial(const non_trivial&) {}
		constexpr non_trivial(non_trivial&&) {}
		constexpr non_trivial& operator=(const non_trivial&)
		{
			return *this;
		}
		constexpr non_trivial& operator=(non_trivial&&)
		{
			return *this;
		}
		constexpr ~non_trivial() {}
	};

	using trivial_tuple = TUPLE_NAMESPACE::tuple<int, float, char>;

	static_assert(std::is_trivial_v<trivial_tuple>);
	static_assert(std::is_trivially_assignable_v<trivial_tuple, trivial_tuple>);
	static_assert(std::is_trivially_constructible_v<trivial_tuple>);
	static_assert(std::is_trivially_copyable_v<trivial_tuple>);
	static_assert(std::is_trivially_copy_constructible_v<trivial_tuple>);
	static_assert(std::is_trivially_default_constructible_v<trivial_tuple>);
	static_assert(std::is_trivially_destructible_v<trivial_tuple>);
	static_assert(std::is_trivially_move_assignable_v<trivial_tuple>);
	static_assert(std::is_trivially_move_constructible_v<trivial_tuple>);

	static_assert(std::is_swappable_v<trivial_tuple>);

	static_assert(std::is_nothrow_assignable_v<trivial_tuple, trivial_tuple>);
	static_assert(std::is_nothrow_constructible_v<trivial_tuple>);
	static_assert(std::is_nothrow_copy_assignable_v<trivial_tuple>);
	static_assert(std::is_nothrow_copy_constructible_v<trivial_tuple>);
	static_assert(std::is_nothrow_default_constructible_v<trivial_tuple>);
	static_assert(std::is_nothrow_destructible_v<trivial_tuple>);

	struct empty_struct{};

	static constexpr auto int_size = sizeof(int);
	static constexpr auto tuple_int_size = sizeof(TUPLE_NAMESPACE::tuple<int>);
	static constexpr auto tuple_empty_size = sizeof(TUPLE_NAMESPACE::tuple<empty_struct>);
	static constexpr auto tuple_empty_empty_size = sizeof(TUPLE_NAMESPACE::tuple<empty_struct, empty_struct>);
	static constexpr auto tuple_int_empty_size = sizeof(TUPLE_NAMESPACE::tuple<int, empty_struct>);
	static constexpr auto tuple_empty_int_size = sizeof(TUPLE_NAMESPACE::tuple<empty_struct, int>);
	static constexpr auto tuple_int_empty_int_size = sizeof(TUPLE_NAMESPACE::tuple<int, empty_struct, int>);
	static constexpr auto tuple_empty_int_empty_size = sizeof(TUPLE_NAMESPACE::tuple<empty_struct, int, empty_struct>);

	static_assert(int_size == tuple_int_size);
	static_assert(int_size == tuple_int_empty_size);
	static_assert(int_size == tuple_empty_int_size);
	static_assert(int_size == tuple_empty_int_empty_size);
	static_assert(int_size * 2 == tuple_int_empty_int_size);
	static_assert(1 == tuple_empty_size);
	static_assert(1 == tuple_empty_empty_size);

	template<typename T1, typename T2, typename T3>
	constexpr bool tuple_element_type_test() noexcept
	{
		using my_tup = TUPLE_NAMESPACE::tuple<T1, T2, T3>;
		using my_lvalue_tup = TUPLE_NAMESPACE::tuple<T1&, T2&, T3&>;
		using my_const_lvalue_tup = TUPLE_NAMESPACE::tuple<const T1&, const T2&, const T3&>;
		using my_rvalue_tup = TUPLE_NAMESPACE::tuple<T1&&, T2&&, T3&&>;
		using my_const_rvalue_tup = TUPLE_NAMESPACE::tuple<const T1&&, const T2&&, const T3&&>;

		static_assert(std::same_as<T1, std::tuple_element_t<0, my_tup>>);
		static_assert(std::same_as<T2, std::tuple_element_t<1, my_tup>>);
		static_assert(std::same_as<T3, std::tuple_element_t<2, my_tup>>);

		static_assert(std::same_as<T1&, std::tuple_element_t<0, my_lvalue_tup>>);
		static_assert(std::same_as<T2&, std::tuple_element_t<1, my_lvalue_tup>>);
		static_assert(std::same_as<T3&, std::tuple_element_t<2, my_lvalue_tup>>);

		static_assert(std::same_as<const T1&, std::tuple_element_t<0, my_const_lvalue_tup>>);
		static_assert(std::same_as<const T2&, std::tuple_element_t<1, my_const_lvalue_tup>>);
		static_assert(std::same_as<const T3&, std::tuple_element_t<2, my_const_lvalue_tup>>);

		static_assert(std::same_as<T1&&, std::tuple_element_t<0, my_rvalue_tup>>);
		static_assert(std::same_as<T2&&, std::tuple_element_t<1, my_rvalue_tup>>);
		static_assert(std::same_as<T3&&, std::tuple_element_t<2, my_rvalue_tup>>);

		static_assert(std::same_as<const T1&&, std::tuple_element_t<0, my_const_rvalue_tup>>);
		static_assert(std::same_as<const T2&&, std::tuple_element_t<1, my_const_rvalue_tup>>);
		static_assert(std::same_as<const T3&&, std::tuple_element_t<2, my_const_rvalue_tup>>);

		return true;
	}

	static_assert(tuple_element_type_test<int, float, char>());

	template <typename T, typename Expected, bool RvalueTuple>
	struct expected
	{
		using type = std::conditional_t<std::is_reference_v<T>,
			std::conditional_t<RvalueTuple, T&&, T&>,
			Expected>;
	};

	template <typename T, typename Expected, bool RvalueTuple>
	using expected_t = typename expected<T, Expected, RvalueTuple>::type;

	template<typename T>
	constexpr bool get_tests() noexcept
	{
		using lvalue_tuple = TUPLE_NAMESPACE::tuple<T>&;
		using const_lvalue_tuple = const TUPLE_NAMESPACE::tuple<T>&;
		using rvalue_tuple = TUPLE_NAMESPACE::tuple<T>&&;
		using const_rvalue_tuple = const TUPLE_NAMESPACE::tuple<T>&&;

		using lvalue_tuple_get_result = decltype(std::get<0>(std::declval<lvalue_tuple>()));
		using const_lvalue_tuple_get_result = decltype(std::get<0>(std::declval<const_lvalue_tuple>()));
		using rvalue_tuple_get_result = decltype(std::get<0>(std::declval<rvalue_tuple>()));
		using const_rvalue_tuple_get_result = decltype(std::get<0>(std::declval<const_rvalue_tuple>()));

		static_assert(std::same_as<expected_t<T, T&, false>, lvalue_tuple_get_result>);
		static_assert(std::same_as<expected_t<T, const T&, false>, const_lvalue_tuple_get_result>);
		static_assert(std::same_as<expected_t<T, T&&, true>, rvalue_tuple_get_result>);
		static_assert(std::same_as<expected_t<T, const T&&, true>, const_rvalue_tuple_get_result>);

		return true;
	}

	template<typename T>
	constexpr bool full_get_tests() noexcept
	{
		static_assert(get_tests<T>());
		static_assert(get_tests<T&>());
		static_assert(get_tests<const T&>());
		static_assert(get_tests<T&&>());
		static_assert(get_tests<const T&&>());

		return true;
	}
	static_assert(full_get_tests<int>());
	static_assert(full_get_tests<non_trivial>());

	static_assert(std::same_as<
		TUPLE_NAMESPACE::tuple<int, float, char>,
		decltype(TUPLE_NAMESPACE::tuple{
			std::declval<int>(),
			std::declval<float>(),
			std::declval<char>()
			})
	> );

	static_assert(std::same_as<
		TUPLE_NAMESPACE::tuple<int, float, char>,
		decltype(TUPLE_NAMESPACE::tuple{
			std::declval<const int>(),
			std::declval<const float>(),
			std::declval<const char>()
			})
	> );

	static_assert(std::same_as <
		TUPLE_NAMESPACE::tuple<int, float, char>,
		decltype(TUPLE_NAMESPACE::tuple{
			std::declval<int&>(),
			std::declval<float&>(),
			std::declval<char&>()
			})
	> );

	static_assert(std::same_as <
		TUPLE_NAMESPACE::tuple<int, float, char>,
		decltype(TUPLE_NAMESPACE::tuple{
			std::declval<int&&>(),
			std::declval<float&&>(),
			std::declval<char&&>()
			})
	> );

	///////////////////////////
	static_assert(std::same_as <
		TUPLE_NAMESPACE::tuple<int&&, float&&, char&&>,
		decltype(TUPLE_NAMESPACE::forward_as_tuple(
			std::declval<int>(),
			std::declval<float>(),
			std::declval<char>()
			))
	> );

	static_assert(std::same_as <
		TUPLE_NAMESPACE::tuple<const int&&, const float&&, const char&&>,
		decltype(TUPLE_NAMESPACE::forward_as_tuple(
			std::declval<const int>(),
			std::declval<const float>(),
			std::declval<const char>()
			))
	> );

	static_assert(std::same_as <
		TUPLE_NAMESPACE::tuple<int&, float&, char&>,
		decltype(TUPLE_NAMESPACE::forward_as_tuple(
			std::declval<int&>(),
			std::declval<float&>(),
			std::declval<char&>()
			))
	> );

	static_assert(std::same_as <
		TUPLE_NAMESPACE::tuple<int&&, float&&, char&&>,
		decltype(TUPLE_NAMESPACE::forward_as_tuple(
			std::declval<int&&>(),
			std::declval<float&&>(),
			std::declval<char&&>()
			))
	> );
}
