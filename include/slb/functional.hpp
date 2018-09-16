/*
  SLB.Functional

  Copyright Michael Park, 2017
  Copyright Agustin Berge, 2017

  Distributed under the Boost Software License, Version 1.0.
  (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)
*/

#ifndef SLB_FUNCTIONAL_HPP
#define SLB_FUNCTIONAL_HPP

/*

Header <functional> synopsis [functional.syn]

namespace std {
  // [func.invoke], invoke
  template <class F, class... Args>
    invoke_result_t<F, Args...> invoke(F&& f, Args&&... args)
      noexcept(is_nothrow_invocable_v<F, Args...>);

  // [refwrap], reference_wrapper
  template <class T> class reference_wrapper;

  template <class T> reference_wrapper<T> ref(T&) noexcept;
  template <class T> reference_wrapper<const T> cref(const T&) noexcept;
  template <class T> void ref(const T&&) = delete;
  template <class T> void cref(const T&&) = delete;

  template <class T> reference_wrapper<T> ref(reference_wrapper<T>) noexcept;
  template <class T> reference_wrapper<const T> cref(reference_wrapper<T>)
      noexcept;

  // [arithmetic.operations], arithmetic operations
  template <class T = void> struct plus;
  template <class T = void> struct minus;
  template <class T = void> struct multiplies;
  template <class T = void> struct divides;
  template <class T = void> struct modulus;
  template <class T = void> struct negate;
  template <> struct plus<void>;
  template <> struct minus<void>;
  template <> struct multiplies<void>;
  template <> struct divides<void>;
  template <> struct modulus<void>;
  template <> struct negate<void>;

  // [comparisons], comparisons
  template <class T = void> struct equal_to;
  template <class T = void> struct not_equal_to;
  template <class T = void> struct greater;
  template <class T = void> struct less;
  template <class T = void> struct greater_equal;
  template <class T = void> struct less_equal;
  template <> struct equal_to<void>;
  template <> struct not_equal_to<void>;
  template <> struct greater<void>;
  template <> struct less<void>;
  template <> struct greater_equal<void>;
  template <> struct less_equal<void>;

  // [logical.operations], logical operations
  template <class T = void> struct logical_and;
  template <class T = void> struct logical_or;
  template <class T = void> struct logical_not;
  template <> struct logical_and<void>;
  template <> struct logical_or<void>;
  template <> struct logical_not<void>;

  // [bitwise.operations], bitwise operations
  template <class T = void> struct bit_and;
  template <class T = void> struct bit_or;
  template <class T = void> struct bit_xor;
  template <class T = void> struct bit_not;
  template <> struct bit_and<void>;
  template <> struct bit_or<void>;
  template <> struct bit_xor<void>;
  template <> struct bit_not<void>;

  // [func.not_fn], function template not_fn
  template <class F> unspecified not_fn(F&& f);

  // [func.bind], bind
  template<class T> struct is_bind_expression;
  template<class T> struct is_placeholder;

  template<class F, class... BoundArgs>
    unspecified bind(F&&, BoundArgs&&...);
  template<class R, class F, class... BoundArgs>
    unspecified bind(F&&, BoundArgs&&...);

  namespace placeholders {
    // M is the implementation-defined number of placeholders
    see below _1;
    see below _2;
    ...
    see below _M;
  }

  // [func.memfn], member function adaptors
  template<class R, class T>
    unspecified mem_fn(R T::*) noexcept;

  // [func.wrap], polymorphic function wrappers
  class bad_function_call;

  template<class> class function; // not defined
  template<class R, class... ArgTypes> class function<R(ArgTypes...)>;

  template<class R, class... ArgTypes>
    bool operator==(const function<R(ArgTypes...)>&, nullptr_t) noexcept;
  template<class R, class... ArgTypes>
    bool operator==(nullptr_t, const function<R(ArgTypes...)>&) noexcept;
  template<class R, class... ArgTypes>
    bool operator!=(const function<R(ArgTypes...)>&, nullptr_t) noexcept;
  template<class R, class... ArgTypes>
    bool operator!=(nullptr_t, const function<R(ArgTypes...)>&) noexcept;

  template<class R, class... ArgTypes>
    void swap(function<R(ArgTypes...)>&, function<R(ArgTypes...)>&) noexcept;

  // [func.search], searchers
  template<class ForwardIterator, class BinaryPredicate = equal_to<>>
    class default_searcher;

  template<class RandomAccessIterator,
           class Hash = hash<
               typename iterator_traits<RandomAccessIterator>::value_type>,
           class BinaryPredicate = equal_to<>>
    class boyer_moore_searcher;

  template<class RandomAccessIterator,
           class Hash = hash<
               typename iterator_traits<RandomAccessIterator>::value_type>,
           class BinaryPredicate = equal_to<>>
    class boyer_moore_horspool_searcher;

  // [unord.hash], hash function primary template
  template <class T>
    struct hash;

  // [func.bind], function object binders
  template <class T>
    inline constexpr bool is_bind_expression_v = is_bind_expression<T>::value;
  template <class T>
    inline constexpr int is_placeholder_v = is_placeholder<T>::value;
}

*/

#include <cassert>
#include <cstddef>
#include <functional>
#include <memory>
#include <new>
#include <type_traits>
#include <typeinfo>
#include <utility>

#include "detail/config.hpp"
#include "detail/invoke.hpp"
#include "type_traits.hpp"

namespace slb {

// [func.invoke], invoke

// We only enable the C++17 implementation under C++2a here to account for
// P0704: "Fixing const-qualified pointers to members".

#if __cpp_lib_invoke /* C++17 */ && __cplusplus > 201703L /* C++2a */
using std::invoke;
#else
template <typename F, typename... Args>
typename slb::invoke_result<F, Args...>::type
invoke(F&& f,
       Args&&... args) noexcept(slb::is_nothrow_invocable<F, Args...>::value) {
  return detail::invoke(std::forward<F>(f), std::forward<Args>(args)...);
}
#endif

// [func.not_fn], function template not_fn

// We only enable the C++17 implementation under C++2a here to account for
// P0704: "Fixing const-qualified pointers to members" and LWG2210: "INVOKE-ing
// a pointer to member with a `reference_wrapper` as the object expression".

#if __cpp_lib_not_fn /* C++17 */ && __cplusplus > 201703L /* C++2a */
using std::not_fn;
#else
namespace detail {
struct not_fn_tag {
  explicit not_fn_tag() = default;
};

template <typename FD>
class not_fn_result {
  FD fd;

public:
  template <typename F>
  not_fn_result(not_fn_tag, F&& f) : fd(std::forward<F>(f)) {}

  not_fn_result(not_fn_result&&) = default;
  not_fn_result(not_fn_result const&) = default;

  template <typename... Args>
      auto operator()(Args&&... args) &
      noexcept(noexcept(!detail::invoke(std::declval<FD&>(),
                                        std::forward<Args>(args)...)))
          -> decltype(!detail::invoke(std::declval<FD&>(),
                                      std::forward<Args>(args)...)) {
    return !detail::invoke(fd, std::forward<Args>(args)...);
  }

  template <typename... Args>
  auto operator()(Args&&... args) const& noexcept(noexcept(
      !detail::invoke(std::declval<FD const&>(), std::forward<Args>(args)...)))
      -> decltype(!detail::invoke(std::declval<FD const&>(),
                                  std::forward<Args>(args)...)) {
    return !detail::invoke(fd, std::forward<Args>(args)...);
  }

  template <typename... Args>
      auto operator()(Args&&... args) &&
      noexcept(noexcept(!detail::invoke(std::declval<FD&&>(),
                                        std::forward<Args>(args)...)))
          -> decltype(!detail::invoke(std::declval<FD&&>(),
                                      std::forward<Args>(args)...)) {
    return !detail::invoke(std::move(fd), std::forward<Args>(args)...);
  }

// gcc finds calls on const rvalues ambiguous up to version 4.8.
#if !defined(__GNUC__) || (__GNUC__ > 4) ||                                    \
    ((__GNUC__ == 4) && (__GNUC_MINOR__ > 8))
  template <typename... Args>
  auto operator()(Args&&... args) const&& noexcept(noexcept(
      !detail::invoke(std::declval<FD const&&>(), std::forward<Args>(args)...)))
      -> decltype(!detail::invoke(std::declval<FD const&&>(),
                                  std::forward<Args>(args)...)) {
    return !detail::invoke(std::move(fd), std::forward<Args>(args)...);
  }
#endif
};
} // namespace detail

template <typename F, typename FD = typename std::decay<F>::type>
detail::not_fn_result<FD> not_fn(F&& f) {
  static_assert(std::is_move_constructible<FD>::value &&
                    std::is_convertible<FD&&, FD>::value,
                "FD shall satisfy the requirements of MoveConstructible");
  static_assert(std::is_constructible<FD, F>::value,
                "is_constructible_v<FD, F> shall be true");
  return {detail::not_fn_tag{}, std::forward<F>(f)};
}
#endif

// [func.bind], bind

#if SLB_INTEGRAL_CONSTANT == 2 // C++14
using std::is_bind_expression;
using std::is_placeholder;
#else
template <typename T>
struct is_bind_expression
    : slb::bool_constant<std::is_bind_expression<T>::value> {};

template <typename T>
struct is_placeholder
    : slb::integral_constant<int, std::is_placeholder<T>::value> {};
#endif

// We only enable the C++17 implementation under C++2a here to account for
// P0704: "Fixing const-qualified pointers to members" and LWG2210: "INVOKE-ing
// a pointer to member with a `reference_wrapper` as the object expression".

#if __cpp_lib_invoke /* C++17 */ && __cplusplus > 201703L /* C++2a */
using std::bind;
#else
namespace detail {
struct bind_tag {
  explicit bind_tag() = default;
};

template <typename FD>
class bound {
  FD fd;

public:
  template <typename F>
  bound(bind_tag, F&& f) : fd(std::forward<F>(f)) {}

  template <typename... Args>
  auto operator()(Args&&... args) noexcept(noexcept(
      detail::invoke(std::declval<FD&>(), std::forward<Args>(args)...)))
      -> decltype(detail::invoke(std::declval<FD&>(),
                                 std::forward<Args>(args)...)) {
    return detail::invoke(fd, std::forward<Args>(args)...);
  }

  template <typename... Args>
  auto operator()(Args&&... args) const
      noexcept(noexcept(detail::invoke(std::declval<FD const&>(),
                                       std::forward<Args>(args)...)))
          -> decltype(detail::invoke(std::declval<FD const&>(),
                                     std::forward<Args>(args)...)) {
    return detail::invoke(fd, std::forward<Args>(args)...);
  }
};

template <typename FD, typename... BoundArgs>
struct bind_result {
  using type = decltype(std::bind(std::declval<detail::bound<FD>>(),
                                  std::declval<BoundArgs>()...));
};

template <typename R, typename FD>
class bound_r {
  FD fd;

public:
  template <typename F>
  bound_r(bind_tag, F&& f) : fd(std::forward<F>(f)) {}

  template <typename... Args>
  auto operator()(Args&&... args) noexcept(noexcept(
      detail::invoke_r<R>(std::declval<FD&>(), std::forward<Args>(args)...)))
      -> decltype(detail::invoke_r<R>(std::declval<FD&>(),
                                      std::forward<Args>(args)...)) {
    return detail::invoke_r<R>(fd, std::forward<Args>(args)...);
  }

  template <typename... Args>
  auto operator()(Args&&... args) const
      noexcept(noexcept(detail::invoke_r<R>(std::declval<FD const&>(),
                                            std::forward<Args>(args)...)))
          -> decltype(detail::invoke_r<R>(std::declval<FD const&>(),
                                          std::forward<Args>(args)...)) {
    return detail::invoke_r<R>(fd, std::forward<Args>(args)...);
  }
};

template <typename R, typename FD, typename... BoundArgs>
struct bind_result_r {
  using type = decltype(std::bind<R>(std::declval<detail::bound_r<R, FD>>(),
                                     std::declval<BoundArgs>()...));
};
} // namespace detail

template <typename F,
          typename... BoundArgs,
          typename FD = typename std::decay<F>::type>
typename detail::bind_result<FD, BoundArgs...>::type bind(F&& f,
                                                          BoundArgs&&... args) {
  return std::bind(detail::bound<FD>{detail::bind_tag{}, std::forward<F>(f)},
                   std::forward<BoundArgs>(args)...);
}

template <typename R,
          typename F,
          typename... BoundArgs,
          typename FD = typename std::decay<F>::type>
typename detail::bind_result_r<R, FD, BoundArgs...>::type
bind(F&& f, BoundArgs&&... args) {
  return std::bind<R>(
      detail::bound_r<R, FD>{detail::bind_tag{}, std::forward<F>(f)},
      std::forward<BoundArgs>(args)...);
}
#endif

namespace placeholders = std::placeholders;

// [func.memfn], member function adaptors

// We only enable the C++17 implementation under C++2a here to account for
// P0704: "Fixing const-qualified pointers to members" and LWG2210: "INVOKE-ing
// a pointer to member with a `reference_wrapper` as the object expression".

#if __cpp_lib_invoke /* C++17 */ && __cplusplus > 201703L /* C++2a */
using std::mem_fn;
#else
template <typename T, typename C>
typename detail::mem_fn_result<T C::*>::type mem_fn(T C::*pm) noexcept {
  return pm;
}
#endif

// [func.wrap], polymorphic function wrappers
using std::bad_function_call;

template <typename>
class function; // not defined

namespace detail {
union function_storage {
  void* obj_ptr;
  void (*fun_ptr)();
  struct incomplete;
  void (incomplete::*mem_ptr)();
};

template <typename T>
union function_embedded_storage {
  T value;
  function_storage _;
};

template <typename T>
struct function_is_embedded
    : std::integral_constant<
          bool,
          sizeof(T) <= sizeof(function_storage) &&
              alignof(function_storage) % alignof(T) == 0 &&
              std::is_move_constructible<function_embedded_storage<T>>::value &&
              std::is_destructible<function_embedded_storage<T>>::value> {};

template <typename T, typename F>
static void function_construct(function_storage& storage, F&& f) noexcept(
    function_is_embedded<T>::value&&
        std::is_nothrow_constructible<T, F>::value) {
  if (function_is_embedded<T>::value) {
    ::new (static_cast<void*>(&storage)) T(std::forward<F>(f));
  } else {
    storage.obj_ptr = new T(std::forward<F>(f));
  }
}

template <typename T>
static void function_move(function_storage& storage, T& f) noexcept {
  if (function_is_embedded<T>::value) {
    ::new (static_cast<void*>(&storage)) T(std::move(f));
    f.~T();
  } else {
    storage.obj_ptr = std::addressof(f);
  }
}

template <typename T>
static void function_destroy(function_storage& storage) noexcept {
  if (function_is_embedded<T>::value) {
    reinterpret_cast<T*>(&storage)->~T();
  } else {
    delete static_cast<T*>(storage.obj_ptr);
  }
}

template <typename T>
static T& function_get(function_storage& storage) noexcept {
  if (function_is_embedded<T>::value) {
    return *reinterpret_cast<T*>(&storage);
  } else {
    return *static_cast<T*>(storage.obj_ptr);
  }
}

enum class function_action {
  get = 1,
  copy = 2,
  move = 3,
  destroy = 4,
  type_id = 5,
};

template <typename T>
inline void* function_manage(function_storage& storage,
                             function_action action,
                             function_storage* source = nullptr) {
  switch (action) {
  case function_action::get:
    return std::addressof(function_get<T>(storage));
  case function_action::copy:
    return function_construct<T>(storage, function_get<T const>(*source)),
           nullptr;
  case function_action::move:
    return function_move<T>(storage, function_get<T>(*source)), nullptr;
  case function_action::destroy:
    return function_destroy<T>(storage), nullptr;
#if SLB_HAS_CXX98_RTTI
  case function_action::type_id:
    return const_cast<std::type_info*>(&typeid(T));
#endif
  }
  return assert(false), nullptr;
}

template <typename T, typename R, typename... ArgTypes>
static R
function_invoke(function_storage& storage, ArgTypes&&... args) noexcept(
    slb::is_nothrow_invocable_r<R, T&, ArgTypes...>::value) {
  T& obj = function_get<T>(storage);
  return detail::invoke_r<R>(obj, std::forward<ArgTypes>(args)...);
}

template <typename R, typename... ArgTypes>
static R throw_bad_function_call(function_storage& /*storage*/,
                                 ArgTypes&&... /*args*/) {
  throw std::bad_function_call();
}

template <typename T>
static bool function_is_empty(T&) noexcept {
  return false;
}
template <typename R, typename... ArgTypes>
static bool function_is_empty(R (*&ptr)(ArgTypes...)) noexcept {
  return ptr == nullptr;
}
template <typename R, typename C>
static bool function_is_empty(R C::*& mem_ptr) noexcept {
  return mem_ptr == nullptr;
}
template <typename T>
static bool function_is_empty(std::function<T>& f) noexcept {
  return !f;
}
} // namespace detail

template <typename R, typename... ArgTypes>
class function<R(ArgTypes...)> {
public:
  using result_type = R;

  // [func.wrap.func.con], construct/copy/destroy
  function() noexcept
      : _manage(nullptr),
        _invoke(&detail::throw_bad_function_call<R, ArgTypes...>) {}

  function(std::nullptr_t) noexcept
      : _manage(nullptr),
        _invoke(&detail::throw_bad_function_call<R, ArgTypes...>) {}

  function(function const& f) : _manage(f._manage), _invoke(f._invoke) {
    if (_manage != nullptr) {
      _manage(_storage, detail::function_action::copy, &f._storage);
    }
  }

  function(function&& f) noexcept : _manage(f._manage), _invoke(f._invoke) {
    if (_manage != nullptr) {
      _manage(_storage, detail::function_action::move, &f._storage);
    }
    f._manage = nullptr;
    f._invoke = &detail::throw_bad_function_call<R, ArgTypes...>;
  }

  template <typename FD,
            typename Enable = decltype((void)(detail::invoke_r<R>(
                std::declval<FD&>(), std::declval<ArgTypes>()...)))>
  function(FD f) noexcept(detail::function_is_embedded<FD>::value&&
                              std::is_nothrow_move_constructible<FD>::value) {
    if (!detail::function_is_empty(f)) {
      detail::function_construct<FD>(_storage, std::move(f));
      _manage = &detail::function_manage<FD>;
      _invoke = &detail::function_invoke<FD, R, ArgTypes...>;
    } else {
      _manage = nullptr;
      _invoke = &detail::throw_bad_function_call<R, ArgTypes...>;
    }
  }

  function& operator=(function const& f) {
    if (f._manage != nullptr) {
      detail::function_storage temp_storage;
      f._manage(temp_storage, detail::function_action::copy, &f._storage);

      if (_manage != nullptr) {
        _manage(_storage, detail::function_action::destroy, nullptr);
      }
      _manage = f._manage;
      _invoke = f._invoke;
      _manage(_storage, detail::function_action::move, &temp_storage);
    } else if (_manage != nullptr) {
      _manage(_storage, detail::function_action::destroy, nullptr);
      _manage = nullptr;
      _invoke = &detail::throw_bad_function_call<R, ArgTypes...>;
    }
    return *this;
  }

  function& operator=(function&& f) noexcept {
    if (_manage != nullptr) {
      _manage(_storage, detail::function_action::destroy, nullptr);
    }
    _manage = f._manage;
    _invoke = f._invoke;
    if (_manage != nullptr) {
      _manage(_storage, detail::function_action::move, &f._storage);
    }
    f._manage = nullptr;
    f._invoke = &detail::throw_bad_function_call<R, ArgTypes...>;
    return *this;
  }

  function& operator=(std::nullptr_t) noexcept {
    if (_manage != nullptr) {
      _manage(_storage, detail::function_action::destroy, nullptr);
    }
    _manage = nullptr;
    _invoke = &detail::throw_bad_function_call<R, ArgTypes...>;
    return *this;
  }

  template <typename F,
            typename FD = typename detail::lib::remove_cvref<F>::type,
            typename Enable = decltype((void)(detail::invoke_r<R>(
                std::declval<FD&>(), std::declval<ArgTypes>()...)))>
  function&
  operator=(F&& f) noexcept(detail::function_is_embedded<FD>::value&&
                                std::is_nothrow_constructible<FD, F>::value) {
    if (!detail::function_is_empty(f)) {
      if (std::is_nothrow_constructible<FD, F>::value) {
        if (_manage != nullptr) {
          _manage(_storage, detail::function_action::destroy, nullptr);
        }
        detail::function_construct<FD>(_storage, std::forward<F>(f));
        _manage = &detail::function_manage<FD>;
        _invoke = &detail::function_invoke<FD, R, ArgTypes...>;
      } else {
        detail::function_storage temp_storage;
        detail::function_construct<FD>(temp_storage, std::forward<F>(f));

        if (_manage != nullptr) {
          _manage(_storage, detail::function_action::destroy, nullptr);
        }
        _manage = &detail::function_manage<FD>;
        _invoke = &detail::function_invoke<FD, R, ArgTypes...>;
        _manage(_storage, detail::function_action::move, &temp_storage);
      }
    } else if (_manage != nullptr) {
      _manage(_storage, detail::function_action::destroy, nullptr);
      _manage = nullptr;
      _invoke = &detail::throw_bad_function_call<R, ArgTypes...>;
    }
    return *this;
  }

  template <typename F, typename FD = std::reference_wrapper<F>>
  function& operator=(std::reference_wrapper<F> f) noexcept {
    if (_manage != nullptr) {
      _manage(_storage, detail::function_action::destroy, nullptr);
    }
    detail::function_construct<FD>(_storage, std::move(f));
    _manage = &detail::function_manage<FD>;
    _invoke = &detail::function_invoke<FD, R, ArgTypes...>;
    return *this;
  }

  ~function() {
    if (_manage != nullptr) {
      _manage(_storage, detail::function_action::destroy, nullptr);
    }
  }

  // [func.wrap.func.mod], function modifiers
  void swap(function& other) noexcept {
    detail::function_storage temp_storage;
    if (_manage != nullptr) {
      _manage(temp_storage, detail::function_action::move, &_storage);
    }
    if (other._manage != nullptr) {
      other._manage(_storage, detail::function_action::move, &other._storage);
    }
    if (_manage != nullptr) {
      _manage(other._storage, detail::function_action::move, &temp_storage);
    }
    std::swap(_manage, other._manage);
    std::swap(_invoke, other._invoke);
  }

  // [func.wrap.func.cap], function capacity
  explicit operator bool() const noexcept { return _manage != nullptr; }

  // [func.wrap.func.inv], function invocation
  R operator()(ArgTypes... args) const {
    return _invoke(_storage, std::forward<ArgTypes>(args)...);
  }

  // [func.wrap.func.targ], function target access
#if SLB_HAS_CXX98_RTTI
  std::type_info const& target_type() const noexcept {
    if (_manage != nullptr) {
      return *static_cast<std::type_info const*>(
          _manage(_storage, detail::function_action::type_id, nullptr));
    }
    return typeid(void);
  }
#endif

  template <typename T>
  T* target() noexcept {
    if (_manage == &detail::function_manage<T>) {
      return static_cast<T*>(
          _manage(_storage, detail::function_action::get, nullptr));
    }
    return nullptr;
  }

  template <typename T>
  T const* target() const noexcept {
    if (_manage == &detail::function_manage<T>) {
      return static_cast<T const*>(
          _manage(_storage, detail::function_action::get, nullptr));
    }
    return nullptr;
  }

private:
  detail::function_storage mutable _storage;
  void* (*_manage)(detail::function_storage&,
                   detail::function_action,
                   detail::function_storage*);
  R (*_invoke)(detail::function_storage&, ArgTypes&&...);
};

#if SLB_HAS_CXX17_DEDUCTION_GUIDES
namespace detail {
template <typename F>
struct function_deduced_type_noexcept {};

template <typename F>
struct function_deduced_type : function_deduced_type_noexcept<F> {};

template <typename R, typename G, typename... A>
struct function_deduced_type_noexcept<R (G::*)(A...) noexcept> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type_noexcept<R (G::*)(A...) const noexcept> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type_noexcept<R (G::*)(A...) volatile noexcept> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type_noexcept<R (G::*)(A...) const volatile noexcept> {
  using type = R(A...);
};

template <typename R, typename G, typename... A>
struct function_deduced_type_noexcept<R (G::*)(A...) & noexcept> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type_noexcept<R (G::*)(A...) const& noexcept> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type_noexcept<R (G::*)(A...) volatile& noexcept> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type_noexcept<R (G::*)(A...) const volatile& noexcept> {
  using type = R(A...);
};

template <typename R, typename G, typename... A>
struct function_deduced_type<R (G::*)(A...)> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type<R (G::*)(A...) const> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type<R (G::*)(A...) volatile> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type<R (G::*)(A...) const volatile> {
  using type = R(A...);
};

template <typename R, typename G, typename... A>
struct function_deduced_type<R (G::*)(A...)&> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type<R (G::*)(A...) const&> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type<R (G::*)(A...) volatile&> {
  using type = R(A...);
};
template <typename R, typename G, typename... A>
struct function_deduced_type<R (G::*)(A...) const volatile&> {
  using type = R(A...);
};
} // namespace detail

template <typename R, typename... ArgTypes>
function(R (*)(ArgTypes...))->function<R(ArgTypes...)>;

template <typename F>
function(F)
    ->function<
        typename detail::function_deduced_type<decltype(&F::operator())>::type>;
#endif

// [func.wrap.func.nullptr], Null pointer comparisons
template <typename R, typename... ArgTypes>
bool operator==(function<R(ArgTypes...)> const& f, std::nullptr_t) noexcept {
  return !f;
}

template <typename R, typename... ArgTypes>
bool operator==(std::nullptr_t, function<R(ArgTypes...)> const& f) noexcept {
  return !f;
}

template <typename R, typename... ArgTypes>
bool operator!=(function<R(ArgTypes...)> const& f, std::nullptr_t) noexcept {
  return !!f;
}

template <typename R, typename... ArgTypes>
bool operator!=(std::nullptr_t, function<R(ArgTypes...)> const& f) noexcept {
  return !!f;
}

// [func.wrap.func.alg], specialized algorithms
template <typename R, typename... ArgTypes>
void swap(function<R(ArgTypes...)>& f1, function<R(ArgTypes...)>& f2) noexcept {
  f1.swap(f2);
}

// [func.bind], function object binders

#if SLB_HAS_CXX14_VARIABLE_TEMPLATES // C++14
template <typename T>
SLB_CXX17_INLINE_VARIABLE constexpr bool is_bind_expression_v =
    std::is_bind_expression<T>::value;

template <typename T>
SLB_CXX17_INLINE_VARIABLE constexpr int is_placeholder_v =
    slb::is_placeholder<T>::value;
#endif

} // namespace slb

#endif // SLB_FUNCTIONAL_HPP
