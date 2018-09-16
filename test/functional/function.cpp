/*
  SLB.Functional

  Copyright Michael Park, 2017
  Copyright Agustin Berge, 2017

  Distributed under the Boost Software License, Version 1.0.
  (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)
*/

#include <slb/functional.hpp>

#include <exception>
#include <type_traits>
#include <utility>

#include "../catch.hpp"

#define CHECK_NESTED(...)                                                      \
  do {                                                                         \
    INFO(__FILE__ "(" << __LINE__ << "): " #__VA_ARGS__);                      \
    check_##__VA_ARGS__;                                                       \
  } while (false)

// [func.wrap], polymorphic function wrappers

// class bad_function_call;

TEST_CASE("bad_function_call", "[func.wrap.badcall]") {
  CHECK(std::is_base_of<std::exception, slb::bad_function_call>::value);
}

TEST_CASE("bad_function_call::bad_function_call()",
          "[func.wrap.badcall.const]") {
  /* bad_function_call() noexcept; */
  CHECK(noexcept(slb::bad_function_call()));
  slb::bad_function_call e;
  CHECK(e.what() != nullptr);
}

// template<class> class function; // not defined
// template<class R, class... ArgTypes> class function<R(ArgTypes...)>;

TEST_CASE("function", "[func.wrap.func]") {
  /* using result_type = R; */ {
    CHECK(std::is_same<std::function<int()>::result_type, int>::value);
  }
}

// template<class R, class... ArgTypes>
//   function(R(*)(ArgTypes...)) -> function<R(ArgTypes...)>;

// template<class F> function(F) -> function<see below >;
#if SLB_HAS_CXX17_DEDUCTION_GUIDES
template <bool IsNothrow = true>
struct Fn {
  int operator()(int base) noexcept(IsNothrow);
};
template <bool IsNothrow = true>
struct Fn_c {
  int operator()(int base) const noexcept(IsNothrow);
};
template <bool IsNothrow = true>
struct Fn_v {
  int operator()(int base) volatile noexcept(IsNothrow);
};
template <bool IsNothrow = true>
struct Fn_cv {
  int operator()(int base) const volatile noexcept(IsNothrow);
};

template <bool IsNothrow = true>
struct Fnl {
  int operator()(int base) & noexcept(IsNothrow);
};
template <bool IsNothrow = true>
struct Fnl_c {
  int operator()(int base) const& noexcept(IsNothrow);
};
template <bool IsNothrow = true>
struct Fnl_v {
  int operator()(int base) volatile& noexcept(IsNothrow);
};
template <bool IsNothrow = true>
struct Fnl_cv {
  int operator()(int base) const volatile& noexcept(IsNothrow);
};

TEST_CASE("function(guide)", "[func.wrap.func]") {
  struct S {
    static int f(int base) noexcept { return base; }
  };
  CHECK(std::is_same<decltype(slb::function(&S::f)),
                     slb::function<int(int)>>::value);

  /* if decltype(&F::operator()) is of the form R(G::*)(A...) cv &opt
   * noexcept-opt for a class type G, then the deduced type is
   * function<R(A...)> */
  {
    CHECK(std::is_same<decltype(slb::function(Fn<true>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fn_c<true>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fn_v<true>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fn_cv<true>{})),
                       slb::function<int(int)>>::value);

    CHECK(std::is_same<decltype(slb::function(Fnl<true>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fnl_c<true>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fnl_v<true>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fnl_cv<true>{})),
                       slb::function<int(int)>>::value);

    CHECK(std::is_same<decltype(slb::function(Fn<false>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fn_c<false>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fn_v<false>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fn_cv<false>{})),
                       slb::function<int(int)>>::value);

    CHECK(std::is_same<decltype(slb::function(Fnl<false>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fnl_c<false>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fnl_v<false>{})),
                       slb::function<int(int)>>::value);
    CHECK(std::is_same<decltype(slb::function(Fnl_cv<false>{})),
                       slb::function<int(int)>>::value);
  }
}
#endif

// [func.wrap.func.con], construct/copy/destroy
TEST_CASE("function::function", "[func.wrap.func.con]") {
  /* function() noexcept; */ {
    slb::function<int(int)> f;
    CHECK(noexcept(slb::function<int(int)>()));
    CHECK(!f);

    /* implicit */ {
      slb::function<int(int)> i = {};
      CHECK(!i);
    }
  }

  /* function(nullptr_t) noexcept; */ {
    slb::function<int(int)> f(nullptr);
    CHECK(noexcept(slb::function<int(int)>()));
    CHECK(!f);

    /* implicit */ {
      slb::function<int(int)> i = nullptr;
      CHECK(!i);
    }
  }

  /* function(const function&); */ {}

  /* function(function&&); */ {}

  /* template<class F> function(F); */ {}
}

TEST_CASE("function::operator=", "[func.wrap.func.con]") {

  /* function& operator=(const function&); */ {}

  /* function& operator=(function&&); */ {}

  /* function& operator=(nullptr_t) noexcept; */ {}

  /* template<class F> function& operator=(F&&); */ {}

  /* template<class F> function& operator=(reference_wrapper<F>) noexcept; */ {}
}

TEST_CASE("function::~function", "[func.wrap.func.con]") {

  /* ~function(); */ {}
}

// [func.wrap.func.mod], function modifiers
TEST_CASE("function::swap", "[func.wrap.func.mod]") {
  /* void swap(function&) noexcept; */
}

// [func.wrap.func.cap], function capacity
TEST_CASE("function::operator bool", "[func.wrap.func.cap]") {
  /* explicit operator bool() const noexcept; */
}

// [func.wrap.func.inv], function invocation
// template<class R, class... ArgTypes>
// R function<R(ArgTypes...)>::operator()(ArgTypes... args) const
struct C {
  int obj;
  C(int val) : obj(val) {}

  int fun(int base) noexcept { return base + 0; }
  int cfun(int base) const noexcept { return base + 1; }
  int lfun(int base) & noexcept { return base + 2; }
  int rfun(int base) && noexcept { return base + 3; }
  int clfun(int base) const& noexcept { return base + 4; }
  int crfun(int base) const&& noexcept { return base + 5; }
};

struct D : C {
  D(int val) : C(val) {}
};

template <typename T>
struct smart_ptr {
  T* ptr;
  smart_ptr(T* ptr) : ptr(ptr) {}
  T& operator*() const noexcept { return *ptr; }
};

template <typename T>
struct identity {
  using type = T;
};

template <typename R, typename... Args, typename... Tn>
void check_fun(typename identity<R>::type const& r,
               slb::function<R(Args...)> const& fun,
               Tn&&... tn) {
  CHECK(fun(std::forward<Tn>(tn)...) == r);
  CHECK(std::is_same<decltype(fun(std::forward<Tn>(tn)...)), R>::value);
}

TEST_CASE("function(mem-obj-ptr)::operator()", "[func.wrap.func.inv]") {
  auto mem = &C::obj;

  /* reference */ {
    C x = {42};
    C& r = x;
    C const& cr = x;
    slb::function<int&(C&)> const lfun(mem);
    slb::function<int && (C &&)> const rfun(mem);
    slb::function<int const&(C const&)> const clfun(mem);
    slb::function<int const && (C const&&)> const crfun(mem);

    CHECK_NESTED(fun(r.obj, lfun, x));
    CHECK_NESTED(fun(r.obj, rfun, std::move(x)));
    CHECK_NESTED(fun(cr.obj, clfun, x));
    CHECK_NESTED(fun(cr.obj, crfun, std::move(x)));

    D d = {42};
    D& rd = d;
    D const& crd = d;

    CHECK_NESTED(fun(rd.obj, lfun, d));
    CHECK_NESTED(fun(rd.obj, rfun, std::move(d)));
    CHECK_NESTED(fun(crd.obj, clfun, d));
    CHECK_NESTED(fun(crd.obj, crfun, std::move(d)));
  }

  /* reference wrapper */ {
    C x = {42};
    std::reference_wrapper<C> r = x;
    std::reference_wrapper<C const> cr = x;
    slb::function<int&(std::reference_wrapper<C>)> const fun(mem);
    slb::function<int const&(std::reference_wrapper<C const>)> const cfun(mem);

    CHECK_NESTED(fun(r.get().obj, fun, x));
    CHECK_NESTED(fun(cr.get().obj, cfun, x));
  }

  /* pointer */ {
    C x = {42};
    C* p = &x;
    C const* cp = &x;
    slb::function<int(C*)> const fun(mem);
    slb::function<int(C const*)> const cfun(mem);

    CHECK_NESTED(fun((*p).obj, fun, &x));
    CHECK_NESTED(fun((*cp).obj, cfun, &x));
  }

  /* smart pointer */ {
    C x = {42};
    smart_ptr<C> p = &x;
    smart_ptr<C const> cp = &x;
    slb::function<int(smart_ptr<C>)> const fun(mem);
    slb::function<int(smart_ptr<C const>)> const cfun(mem);

    CHECK_NESTED(fun((*p).obj, fun, &x));
    CHECK_NESTED(fun((*cp).obj, cfun, &x));
  }
}

TEST_CASE("function(mem-fun-ptr)::operator()", "[func.wrap.func.inv]") {
  auto mem = &C::fun;
  auto cmem = &C::cfun;
  auto lmem = &C::lfun;
  auto rmem = &C::rfun;
  auto clmem = &C::clfun;
  auto crmem = &C::crfun;

  /* reference */ {
    C x = {42};
    C& r = x;
    C const& cr = x;
    slb::function<int(C, int)> const fun(mem);
    slb::function<int(C const, int)> const cfun(cmem);
    slb::function<int(C&, int)> const lfun(lmem);
    slb::function<int(C&&, int)> const rfun(rmem);
    slb::function<int(C const&, int)> const clfun(clmem);
    slb::function<int(C const&&, int)> const crfun(crmem);

    CHECK_NESTED(fun(r.fun(40), fun, x, 40));
    CHECK_NESTED(fun(cr.cfun(40), cfun, x, 40));
    CHECK_NESTED(fun(r.lfun(40), lfun, x, 40));
    CHECK_NESTED(fun(std::move(r).rfun(40), rfun, std::move(x), 40));
    CHECK_NESTED(fun(cr.clfun(40), clfun, x, 40));
    CHECK_NESTED(fun(std::move(cr).crfun(40), crfun, std::move(x), 40));
  }

  /* reference wrapper */ {
    C x = {42};
    std::reference_wrapper<C> r = x;
    std::reference_wrapper<C const> cr = x;
    slb::function<int(std::reference_wrapper<C>, int)> const fun(mem);
    slb::function<int(std::reference_wrapper<C const>, int)> const cfun(cmem);

    CHECK_NESTED(fun(r.get().fun(40), fun, x, 40));
    CHECK_NESTED(fun(cr.get().cfun(40), cfun, x, 40));
  }

  /* pointer */ {
    C x = {42};
    C* p = &x;
    C const* cp = &x;
    slb::function<int(C*, int)> const fun(mem);
    slb::function<int(C const*, int)> const cfun(cmem);

    CHECK_NESTED(fun((*p).fun(40), fun, &x, 40));
    CHECK_NESTED(fun((*cp).cfun(40), cfun, &x, 40));
  }

  /* smart pointer */ {
    C x = {42};
    smart_ptr<C> p = &x;
    smart_ptr<C const> cp = &x;
    slb::function<int(smart_ptr<C>, int)> const fun(mem);
    slb::function<int(smart_ptr<C const>, int)> const cfun(cmem);

    CHECK_NESTED(fun((*p).fun(40), fun, &x, 40));
    CHECK_NESTED(fun((*cp).cfun(40), cfun, &x, 40));
  }
}

TEST_CASE("function(fun-obj)::operator()", "[func.wrap.func.inv]") {
  /* call-op */ {
    struct Fn {
      int operator()(int base) noexcept { return base + 6; }
      int operator()(int base) const noexcept { return base + 7; }
    };
    auto f = Fn{};
    auto const& fc = f;
    slb::function<int(int)> fun(fc);

    CHECK_NESTED(fun(f(40), fun, 40));
  }

  /* fun-ptr */ {
    struct S {
      static int f(int base) noexcept { return base + 7; }
    };
    auto f = &S::f;
    slb::function<int(int)> const fun(f);

    CHECK_NESTED(fun(f(40), fun, 40));
  }
}

// [func.wrap.func.targ], function target access
TEST_CASE("function::target_type", "[func.wrap.func.targ]") {
  /* const type_info& target_type() const noexcept; */
}

// [func.wrap.func.targ], function target access
TEST_CASE("function::target", "[func.wrap.func.targ]") {
  /* template<class T> T* target() noexcept; */ {}

  /* template<class T> const T* target() const noexcept; */ {}
}

// [func.wrap.func.nullptr], Null pointer comparisons
TEST_CASE("function(comparison)", "[func.wrap.func.nullptr]") {
  // template<class R, class... ArgTypes>
  // bool operator==(const function<R(ArgTypes...)>&, nullptr_t) noexcept;
  // template<class R, class... ArgTypes>
  // bool operator==(nullptr_t, const function<R(ArgTypes...)>&) noexcept;
  // template<class R, class... ArgTypes>
  // bool operator!=(const function<R(ArgTypes...)>&, nullptr_t) noexcept;
  // template<class R, class... ArgTypes>
  // bool operator!=(nullptr_t, const function<R(ArgTypes...)>&) noexcept;
}

// [func.wrap.func.alg], specialized algorithms
TEST_CASE("swap(function, function)", "[func.wrap.func.alg]") {
  // template<class R, class... ArgTypes>
  // void swap(function<R(ArgTypes...)>&, function<R(ArgTypes...)>&) noexcept;
}
