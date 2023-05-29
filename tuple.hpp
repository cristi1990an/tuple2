#pragma once

#include <type_traits>
#include <concepts>
#include <tuple>

#define TUPLE_NAMESPACE std2

namespace TUPLE_NAMESPACE
{
	template<typename ... Args>
	class tuple;
}

namespace std
{
	template<typename ... Types>
	struct tuple_size<TUPLE_NAMESPACE::tuple<Types...>>
		: std::integral_constant<std::size_t, sizeof...(Types)> { };

	template<std::size_t I, typename T>
	struct tuple_element;

	template<std::size_t I, typename Head, typename... Tail>
	struct tuple_element<I, TUPLE_NAMESPACE::tuple<Head, Tail...>>
		: std::tuple_element<I - 1, TUPLE_NAMESPACE::tuple<Tail...>> { };

	template<typename Head, typename... Tail>
	struct tuple_element<0, TUPLE_NAMESPACE::tuple<Head, Tail...>> {
		using type = Head;
	};
}

namespace TUPLE_NAMESPACE
{
	namespace detail
	{
		template <typename T, typename Alloc, typename ... Args>
		struct is_nothrow_constructible_if_uses_allocator :
			std::conditional_t<std::uses_allocator_v<T, Alloc>,
			std::is_nothrow_constructible<T, Alloc, Args...>,
			std::is_nothrow_constructible<T, Args...>> {};

		template <typename T, typename Alloc, typename ... Args>
		static constexpr bool is_nothrow_constructible_if_uses_allocator_v =
			is_nothrow_constructible_if_uses_allocator<T, Alloc, Args...>::value;

		template <typename T, typename Alloc, typename ... Args>
		constexpr T conditionally_create_with_allocator(const Alloc& a, Args&& ... args)
			noexcept (is_nothrow_constructible_if_uses_allocator_v<T, Alloc, Args...>)
		{
			if constexpr (std::uses_allocator_v<T, Alloc>)
			{
				return T(a, std::forward<Args>(args)...);
			}
			else
			{
				return T(std::forward<Args>(args)...);
			}
		}

		template<typename T, typename Arg1, typename ... Args>
		concept forwarding_constructor_guard =
			((!std::same_as<T, std::remove_cvref_t<Arg1>>) ||
			(sizeof...(Args) != 0)) &&
			(!std::same_as<Arg1, std::allocator_arg_t>);

		template<typename T, std::size_t I>
		struct value_wrapper_;

		template <typename T, std::size_t I>
			requires std::is_class_v<T>
		struct value_wrapper_<T, I> : private T
		{
			constexpr value_wrapper_() = default;

			template<typename ... Args>
				requires std::constructible_from<T, Args...> &&
				forwarding_constructor_guard<value_wrapper_, Args...>
				constexpr value_wrapper_(Args&& ... args)
				: T(std::forward<Args>(args)...)
			{

			}

			template<typename Alloc, typename ... Args>
				requires (std::uses_allocator_v<T, Alloc>
					? std::constructible_from<T, Alloc, Args...>
					: std::constructible_from<T, Args...>)
				constexpr value_wrapper_(const Alloc& a, Args&& ... args)
				: T(conditionally_create_with_allocator<T, Alloc, Args...>(a, std::forward<Args>(args)...))
			{

			}

			constexpr T& get_() & noexcept
			{
				return static_cast<T&>(*this);
			}

			constexpr const T& get_() const& noexcept
			{
				return static_cast<const T&>(*this);
			}

			constexpr T&& get_() && noexcept
			{
				return std::move(static_cast<T&>(*this));
			}

			constexpr const T&& get_() const&& noexcept
			{
				return std::move(static_cast<const T&>(*this));
			}
		};

		template <typename T, std::size_t I>
			requires (!std::is_class_v<T>)
		struct value_wrapper_<T, I>
		{
			constexpr value_wrapper_() noexcept = default;
			template <typename U>
			requires std::constructible_from<T, U>
			constexpr value_wrapper_(U&& value) noexcept
				: value_(std::forward<U>(value)) {}

			constexpr value_wrapper_(std::allocator_arg_t, const auto&) noexcept {}
			template <typename U>
			requires std::constructible_from<T, U>
			constexpr value_wrapper_(std::allocator_arg_t, const auto&, U&& value) noexcept
				: value_(std::forward<U>(value)) {}

			constexpr decltype(auto) get_() & noexcept
			{
				return static_cast<std::remove_reference_t<T>&>(value_);
			}

			constexpr decltype(auto) get_() const& noexcept
			{
				if constexpr (std::is_reference_v<T>)
				{
					return static_cast<std::remove_reference_t<T>&>(value_);
				}
				else
				{
					return (value_);
				}
			}

			constexpr decltype(auto) get_() && noexcept
			{
				return static_cast<T&&>(value_);
			}

			constexpr decltype(auto) get_() const&& noexcept
			{
				if constexpr (std::is_reference_v<T>)
				{
					return static_cast<T&&>(value_);
				}
				else
				{
				return std::move(value_);
			}
			}

			T value_;
		};

		template<std::size_t Index, typename ... Args>
		struct tuple_entry_;

		template<std::size_t Index, typename ... Args>
			requires (Index != sizeof...(Args) - 1)
		struct tuple_entry_<Index, Args...>
			: public value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>
			, public tuple_entry_<Index + 1, Args ... >
		{
			constexpr decltype(auto) get_wrapper_() & noexcept
			{
				using wrapper_t = value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>;
				return static_cast<wrapper_t&>(*this);
			}

			constexpr decltype(auto) get_wrapper_() const& noexcept
			{
				using wrapper_t = value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>;
				return static_cast<const wrapper_t&>(*this);
			}

			constexpr decltype(auto) get_wrapper_() const&& noexcept
			{
				using wrapper_t = value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>;
				return static_cast<const wrapper_t&&>(*this);
			}

			constexpr decltype(auto) get_wrapper_() && noexcept
			{
				using wrapper_t = value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>;
				return static_cast<wrapper_t&&>(*this);
			}
		};

		template<std::size_t Index, typename ... Args>
			requires (Index == sizeof...(Args) - 1)
		struct tuple_entry_<Index, Args...>
			: public value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>
		{
			constexpr decltype(auto) get_wrapper_() & noexcept
			{
				using wrapper_t = value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>;
				return static_cast<wrapper_t&>(*this);
			}

			constexpr decltype(auto) get_wrapper_() const& noexcept
			{
				using wrapper_t = value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>;
				return static_cast<const wrapper_t&>(*this);
			}

			constexpr decltype(auto) get_wrapper_() const&& noexcept
			{
				using wrapper_t = value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>;
				return static_cast<const wrapper_t&&>(*this);
			}

			constexpr decltype(auto) get_wrapper_() && noexcept
			{
				using wrapper_t = value_wrapper_<std::tuple_element_t<Index, tuple<Args...>>, Index>;
				return static_cast<wrapper_t&&>(*this);
			}
		};

		template<typename>
		struct is_tuple_spec : std::false_type {};

		template<typename ... Args>
		struct is_tuple_spec<tuple<Args...>> : std::true_type {};

		template<typename T>
		static constexpr bool is_tuple_spec_v = is_tuple_spec<T>::value;

		template<typename T>
		concept tuple_spec = is_tuple_spec_v<std::remove_cvref_t<T>>;

		template<typename T>
		concept boolean_testable = requires (T t)
		{
			[](bool){/**/}(t);
		};

		static constexpr auto synth_three_way =
		[]<typename T, typename U>(const T & t, const U & u) requires
			requires {
				{ t < u } -> boolean_testable;
				{ u < t } -> boolean_testable;
			}
		{
			if constexpr (std::three_way_comparable_with<T, U>)
				return t <=> u;
			else
			{
				if (t < u)
					return std::weak_ordering::less;
				if (u < t)
					return std::weak_ordering::greater;
				return std::weak_ordering::equivalent;
			}
		};

		template <typename T, typename U = T>
		using synth_three_way_result =
			decltype(synth_three_way(std::declval<T&>(), std::declval<U&>()));

		template <typename T, typename Alloc, typename Tuple>
		constexpr T make_from_tuple_with_allocator(const Alloc& a, Tuple&& tup)
		{
			return[&]<std::size_t ... I>(std::index_sequence<I...>)
			{
				return T(a, std::get<I>(std::forward<Tuple>(tup)...));
			}(std::make_index_sequence<std::tuple_size_v<Tuple>>);
		}
	}
}

namespace std
{
	template<std::size_t I, TUPLE_NAMESPACE::detail::tuple_spec Tuple>
	constexpr decltype(auto) get(Tuple&& t) noexcept;
}

namespace TUPLE_NAMESPACE
{
	template <typename ... Args>
	class tuple : public detail::tuple_entry_<0, Args...>
	{
		using underlying_ = detail::tuple_entry_<0, Args...>;

	public:
		constexpr tuple()
			noexcept((std::is_nothrow_default_constructible_v<Args> && ...)) = default;
		constexpr tuple(const tuple&)
			noexcept((std::is_nothrow_copy_constructible_v<Args> && ...)) = default;
		constexpr tuple(tuple&&)
			noexcept((std::is_nothrow_move_constructible_v<Args> && ...)) = default;
		constexpr tuple& operator=(const tuple&)
			noexcept((std::is_nothrow_copy_assignable_v<Args> && ...)) = default;
		constexpr tuple& operator=(tuple&&)
			noexcept((std::is_nothrow_move_assignable_v<Args> && ...)) = default;
		constexpr ~tuple()
			noexcept((std::is_nothrow_destructible_v<Args> && ...)) = default;

		constexpr explicit tuple(const Args& ... args)
			noexcept((std::is_nothrow_copy_constructible_v<Args> && ...))
			: underlying_{ args... }
		{

		}

		template<typename ... Types>
			requires detail::forwarding_constructor_guard<tuple, Types...>
		constexpr explicit tuple(Types&& ... args)
			noexcept((std::is_nothrow_constructible_v<Args, Types> && ...))
			: underlying_{ std::forward<Types>(args) ... }
		{

		}

		template <typename Alloc>
		constexpr tuple(std::allocator_arg_t, const Alloc& a)
			noexcept((detail::is_nothrow_constructible_if_uses_allocator_v<Args, Alloc> && ...))
			: underlying_{ std::allocator_arg, a }
		{

		}

		template <typename Alloc>
		constexpr tuple(std::allocator_arg_t, const Alloc& a, const Args& ... args)
			noexcept((detail::is_nothrow_constructible_if_uses_allocator_v<Args, Alloc, Args> && ...))
			: underlying_{ std::allocator_arg, a, args... }
		{

		}

		template <typename Alloc, typename ... Types>
		constexpr tuple(std::allocator_arg_t, const Alloc& a, Types&& ... args)
			noexcept((detail::is_nothrow_constructible_if_uses_allocator_v<Args, Alloc, Types&&> && ...))
			: underlying_{ std::allocator_arg, a, std::forward<Types>(args)... }
		{

		}

		template <typename Alloc, typename ... Types>
		constexpr tuple(std::allocator_arg_t, const Alloc& a, tuple<Types...>& tup)
			noexcept((detail::is_nothrow_constructible_if_uses_allocator_v<Args, Alloc, Types&> && ...))
			: underlying_{ detail::make_from_tuple_with_allocator<underlying_>(a, tup) }
		{

		}

		template <typename Alloc, typename ... Types>
		constexpr tuple(std::allocator_arg_t, const Alloc& a, const tuple<Types...>& tup)
			noexcept((detail::is_nothrow_constructible_if_uses_allocator_v<Args, Alloc, const Types&> && ...))
			: underlying_{ detail::make_from_tuple_with_allocator<underlying_>(a, tup) }
		{

		}

		template <typename Alloc, typename ... Types>
		constexpr tuple(std::allocator_arg_t, const Alloc& a, tuple<Types...>&& tup)
			noexcept((detail::is_nothrow_constructible_if_uses_allocator_v<Args, Alloc, Types&&> && ...))
			: underlying_{ detail::make_from_tuple_with_allocator<underlying_>(a, std::move(tup)) }
		{

		}

		template <typename Alloc, typename ... Types>
		constexpr tuple(std::allocator_arg_t, const Alloc& a, const tuple<Types...>&& tup)
			noexcept((detail::is_nothrow_constructible_if_uses_allocator_v<Args, Alloc, const Types&&> && ...))
			: underlying_{ detail::make_from_tuple_with_allocator<underlying_>(a, std::move(tup)) }
		{

		}

		template <typename Alloc, typename Tup>
		constexpr tuple(std::allocator_arg_t, const Alloc& a, Tup&& tup)
			// IMPLEMENT NOEXCEPT
			: underlying_{ detail::make_from_tuple_with_allocator<underlying_>(a, std::forward<Tup>(tup)) }
		{

		}

		constexpr void swap(tuple& other)
			noexcept ((std::is_nothrow_swappable_v <Args> && ...))
			requires (std::is_swappable_v<Args> && ...)
		{
			[&] <std::size_t ... I>(std::index_sequence<I...>)
			{
				((std::swap(std::get<I>(*this), std::get<I>(other))), ...);
			}(std::make_index_sequence<sizeof...(Args)>());
		}

		constexpr void swap(const tuple& other) const
			noexcept ((std::is_nothrow_swappable_v <const Args> && ...))
			requires (std::is_swappable_v<const Args> && ...)
		{
			[&] <std::size_t ... I>(std::index_sequence<I...>)
			{
				((std::swap(std::get<I>(*this), std::get<I>(other))), ...);
			}(std::make_index_sequence<sizeof...(Args)>());
		}

	private:
		template<std::size_t I>
		constexpr decltype(auto) get_entry_() & noexcept
		{
			using entry_t = detail::tuple_entry_<I, Args...>;
			return static_cast<entry_t&>(*this);
		}

		template<std::size_t I>
		constexpr decltype(auto) get_entry_() const& noexcept
		{
			using entry_t = detail::tuple_entry_<I, Args...>;
			return static_cast<const entry_t&>(*this);
		}

		template<std::size_t I>
		constexpr decltype(auto) get_entry_() const&& noexcept
		{
			using entry_t = detail::tuple_entry_<I, Args...>;
			return static_cast<const entry_t&&>(*this);
		}

		template<std::size_t I>
		constexpr decltype(auto) get_entry_() && noexcept
		{
			using entry_t = detail::tuple_entry_<I, Args...>;
			return static_cast<entry_t&&>(*this);
		}

		template<std::size_t I>
		constexpr decltype(auto) get_() & noexcept
		{
			return get_entry_<I>().get_wrapper_().get_();
		}

		template<std::size_t I>
		constexpr decltype(auto) get_() const& noexcept
		{
			return get_entry_<I>().get_wrapper_().get_();
		}

		template<std::size_t I>
		constexpr decltype(auto) get_() && noexcept
		{
			return std::move(*this).template get_entry_<I>().get_wrapper_().get_();
		}

		template<std::size_t I>
		constexpr decltype(auto) get_() const&& noexcept
		{
			return std::move(*this).template get_entry_<I>().get_wrapper_().get_();
		}

		template<std::size_t I, TUPLE_NAMESPACE::detail::tuple_spec Tuple>
		friend constexpr decltype(auto) std::get(Tuple&& t) noexcept;
	};

	template <typename ... Types>
	constexpr TUPLE_NAMESPACE::tuple<std::unwrap_ref_decay_t<Types>...>
		make_tuple(Types&& ... args) 
	noexcept ((std::is_nothrow_constructible_v<std::unwrap_ref_decay_t<Types>, Types&&> && ...))
	{
		return TUPLE_NAMESPACE::tuple<std::unwrap_ref_decay_t<Types>...>(
			std::forward<Types>(args)...);
	}

	template <typename ... Types>
	constexpr TUPLE_NAMESPACE::tuple<Types&...> tie(Types& ... args) noexcept
	{
		return { args... };
	}

	template <typename ... Types>
	constexpr TUPLE_NAMESPACE::tuple<Types&& ...> forward_as_tuple(Types&&... args) noexcept
	{
		return { args... };
	}

	template <typename ... Types>
	TUPLE_NAMESPACE::tuple(Types...)->TUPLE_NAMESPACE::tuple<Types...>;

	template <typename T, typename U>
	TUPLE_NAMESPACE::tuple(std::pair<T, U>)->tuple<T, U>;
}

namespace std
{
	template<std::size_t I, TUPLE_NAMESPACE::detail::tuple_spec Tuple>
	constexpr decltype(auto) get(Tuple&& t) noexcept
	{
		return std::forward<Tuple>(t).template get_<I>();
	}

	template <typename ... Types>
		requires (std::is_swappable_v<Types> && ...)
	constexpr void swap(TUPLE_NAMESPACE::tuple<Types...>& lhs,
		                TUPLE_NAMESPACE::tuple<Types...>& rhs)
		noexcept ((std::is_nothrow_swappable_v<Types> && ...))
	{
		lhs.swap(rhs);
	}

	template <typename ... Types>
		requires (std::is_swappable_v<const Types> && ...)
	constexpr void swap(const TUPLE_NAMESPACE::tuple<Types...>& lhs,
		                const TUPLE_NAMESPACE::tuple<Types...>& rhs)
		noexcept ((std::is_nothrow_swappable_v<const Types> && ...))
	{
		lhs.swap(rhs);
	}
}

template <typename ... Types, typename Alloc>
struct std::uses_allocator<TUPLE_NAMESPACE::tuple<Types...>, Alloc> : std::true_type {};

namespace TUPLE_NAMESPACE::detail
{
	template<std::size_t I, std::size_t MaxIndex>
	constexpr auto recursive_compare(const auto& lhs, const auto& rhs) noexcept
	{
		if constexpr (I < MaxIndex - 1)
		{
			const auto result = TUPLE_NAMESPACE::detail::synth_three_way(
				std::get<I>(lhs),
				std::get<I>(rhs));
			return result != 0 ? result
				: recursive_compare<I + 1, MaxIndex>(lhs, rhs);
		}
		else
		{
			return TUPLE_NAMESPACE::detail::synth_three_way(
				std::get<I>(lhs),
				std::get<I>(rhs));
		}
	};
}

template <typename ... TTypes, typename ... UTypes>
constexpr std::common_comparison_category_t<
	TUPLE_NAMESPACE::detail::synth_three_way_result<TTypes, UTypes>...>
	operator<=>(const TUPLE_NAMESPACE::tuple<TTypes...>& lhs,
		const TUPLE_NAMESPACE::tuple<UTypes...>& rhs) noexcept
{
	if constexpr (sizeof...(TTypes) == 0)
	{
		return std::strong_ordering::equal;
	}
	else
	{
		return TUPLE_NAMESPACE::detail::recursive_compare<0, sizeof...(TTypes)>(lhs, rhs);
	}
}
