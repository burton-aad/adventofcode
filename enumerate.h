
/*
  Test implemenatation enumerate.
  based on http://www.reedbeta.com/blog/python-like-enumerate-in-cpp17
  but this version is valid in c++11.
  Plus add a helper so passing a reference to it make it update the reference
  to simplify the usage in c++11 without structured binding.
*/

#include <type_traits>

template <typename I, typename T,
          typename TIter = decltype(std::begin(std::declval<T>())),
          typename = decltype(std::end(std::declval<T>()))>
struct iterator
{
	I i;
	TIter iter;
	bool operator != (const iterator & other) const { return iter != other.iter; }
	void operator ++ () { ++i; ++iter; }

	template <typename Itype = I,
	          typename = typename std::enable_if<std::is_reference<Itype>::value>::type>
	decltype(*iter) operator * () const { return *iter; }
	template <typename Itype = I,
	          typename = typename std::enable_if<not std::is_reference<Itype>::value>::type>
	std::tuple<Itype, decltype(*iter)> operator * () const { return std::tie(i, *iter); }
};

template <typename I, typename T>
struct iterable_wrapper
{
	I i;
	T iterable;
	iterator<I, T> begin() { return iterator<I, T>{ std::forward<I>(i), std::begin(iterable) }; }
	iterator<I, T> end() { return iterator<I, T>{ std::forward<I>(i), std::end(iterable) }; }
};

template <typename I, typename T>
constexpr iterable_wrapper<I, T> enumerate(I && i, T && iterable)
{
	return iterable_wrapper<I, T>{ std::forward<I>(i), std::forward<T>(iterable) };
}

template <typename T>
constexpr iterable_wrapper<size_t, T> enumerate(T && iterable)
{
	return iterable_wrapper<size_t, T>{ 0, std::forward<T>(iterable) };
}
