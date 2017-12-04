/*
  SLB.TypeTraits

  Copyright Michael Park, 2017
  Copyright Agustin Berge, 2017

  Distributed under the Boost Software License, Version 1.0.
  (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)
*/

#include <slb/type_traits.hpp>

#include <type_traits>

#define CATCH_CONFIG_MAIN
#include "catch.hpp"

// 23.15.3, helper class

// template <class T, T v>
// struct integral_constant;

template <typename T, T V>
T deduce_std_integral_constant(std::integral_constant<T, V>) {
  return V;
}

TEST_CASE("integral_constant", "[meta.help]") {
  /* using value_type = T; */ {
    CHECK(std::is_same<slb::integral_constant<int, 0>::value_type, int>::value);
  }

  /* using type = integral_constant; */ {
    CHECK(std::is_same<slb::integral_constant<int, 0>::type,
                       slb::integral_constant<int, 0>>::value);
  }

  /* constexpr operator value_type() const noexcept; */ {
    slb::integral_constant<int, 0> ic;
    CHECK(std::is_convertible<decltype(ic), int>::value);
    CHECK(noexcept(static_cast<int>(ic)));
    CHECK(static_cast<int>(ic) == 0);
    constexpr int ic_value = ic;
    CHECK(ic_value == 0);
  }

  /* constexpr value_type operator()() const noexcept; */ {
    slb::integral_constant<int, 0> ic;
    CHECK(std::is_same<decltype(ic()), int>::value);
    CHECK(noexcept(ic()));
    CHECK(ic() == 0);
    constexpr int ic_value = ic();
    CHECK(ic_value == 0);
  }

  /* std-compatible */ {
    slb::integral_constant<int, 0> slb_ic;
    std::integral_constant<int, 0> std_ic = slb_ic;
    (void)std_ic;

    CHECK(deduce_std_integral_constant(slb_ic) == 0);
  }
}

// template <bool B>
// using bool_constant = integral_constant<bool, B>;

#if __cpp_lib_bool_constant
template <bool B>
bool deduce_std_bool_constant(std::bool_constant<B>) {
  return B;
}
#endif

template <bool B>
bool deduce_std_bool_integral_constant(std::integral_constant<bool, B>) {
  return B;
}

TEST_CASE("bool_constant", "[meta.help]") {
  CHECK(std::is_same<slb::bool_constant<true>::type,
                     slb::integral_constant<bool, true>>::value);

  /* using true_type = bool_constant<true>; */ {
    CHECK(std::is_same<slb::true_type, slb::bool_constant<true>>::value);
  }

  /* using false_type = bool_constant<false>; */ {
    CHECK(std::is_same<slb::false_type, slb::bool_constant<false>>::value);
  }

  /* std-compatible */ {
    slb::integral_constant<bool, true> ic_true;
    std::integral_constant<bool, true> std_true = ic_true;
    (void)std_true;

#if __cpp_lib_bool_constant
    CHECK(deduce_std_bool_constant(ic_true) == true);
#endif
    CHECK(deduce_std_bool_integral_constant(ic_true) == true);

    slb::integral_constant<bool, false> ic_false;
    std::integral_constant<bool, false> std_false = ic_false;
    (void)std_false;

#if __cpp_lib_bool_constant
    CHECK(deduce_std_bool_constant(ic_false) == false);
#endif
    CHECK(deduce_std_bool_integral_constant(ic_false) == false);
  }
}

// 23.15.4.1, primary type categories

// template <class T> struct is_void;
TEST_CASE("is_void", "[meta.unary.cat]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_void<void>>::value);
}

// template <class T> struct is_integral;
TEST_CASE("is_integral", "[meta.unary.cat]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_integral<int>>::value);
}

// template <class T> struct is_floating_point;
TEST_CASE("is_floating_point", "[meta.unary.cat]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_floating_point<double>>::value);
}

// template <class T> struct is_array;
TEST_CASE("is_array", "[meta.unary.cat]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_array<int[3]>>::value);
}

// template <class T> struct is_pointer;
TEST_CASE("is_pointer", "[meta.unary.cat]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_pointer<int*>>::value);
}

// template <class T> struct is_lvalue_reference;
TEST_CASE("is_lvalue_reference", "[meta.unary.cat]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_lvalue_reference<int&>>::value);
}

// template <class T> struct is_rvalue_reference;
TEST_CASE("is_rvalue_reference", "[meta.unary.cat]") {
  CHECK(
      std::is_base_of<slb::true_type, slb::is_rvalue_reference<int&&>>::value);
}

// template <class T> struct is_member_object_pointer;
TEST_CASE("is_member_object_pointer", "[meta.unary.cat]") {
  class C {};
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_member_object_pointer<int C::*>>::value);
}

// template <class T> struct is_member_function_pointer;
TEST_CASE("is_member_function_pointer", "[meta.unary.cat]") {
  class C {};
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_member_function_pointer<int (C::*)()>>::value);
}

// template <class T> struct is_enum;
TEST_CASE("is_enum", "[meta.unary.cat]") {
  enum E {};
  CHECK(std::is_base_of<slb::true_type, slb::is_enum<E>>::value);
}

// template <class T> struct is_union;
TEST_CASE("is_union", "[meta.unary.cat]") {
  union U {};
  CHECK(std::is_base_of<slb::true_type, slb::is_union<U>>::value);
}

// template <class T> struct is_class;
TEST_CASE("is_class", "[meta.unary.cat]") {
  class C {};
  CHECK(std::is_base_of<slb::true_type, slb::is_class<C>>::value);
}

// template <class T> struct is_function;
TEST_CASE("is_function", "[meta.unary.cat]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_function<void()>>::value);
}

// template <class T> struct is_null_pointer;
TEST_CASE("is_null_pointer", "[meta.unary.cat]") {
  using nullptr_t = decltype(nullptr);
  CHECK(
      std::is_base_of<slb::true_type, slb::is_null_pointer<nullptr_t>>::value);

  {
    CHECK(slb::is_null_pointer<nullptr_t>::value);
    CHECK(slb::is_null_pointer<nullptr_t const>::value);
    CHECK(slb::is_null_pointer<nullptr_t volatile>::value);
    CHECK(slb::is_null_pointer<nullptr_t const volatile>::value);
    CHECK_FALSE(slb::is_null_pointer<int>::value);
  }
}

// 23.15.4.2, composite type categories

// template <class T> struct is_reference;
TEST_CASE("is_reference", "[meta.unary.comp]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_reference<int&>>::value);
}

// template <class T> struct is_arithmetic;
TEST_CASE("is_arithmetic", "[meta.unary.comp]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_arithmetic<int>>::value);
}

// template <class T> struct is_fundamental;
TEST_CASE("is_fundamental", "[meta.unary.comp]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_fundamental<int>>::value);
}

// template <class T> struct is_object;
TEST_CASE("is_object", "[meta.unary.comp]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_object<int>>::value);
}

// template <class T> struct is_scalar;
TEST_CASE("is_scalar", "[meta.unary.comp]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_scalar<int>>::value);
}

// template <class T> struct is_compound;
TEST_CASE("is_compound", "[meta.unary.comp]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_compound<int[3]>>::value);
}

// template <class T> struct is_member_pointer;
TEST_CASE("is_member_pointer", "[meta.unary.comp]") {
  class C {};
  CHECK(
      std::is_base_of<slb::true_type, slb::is_member_pointer<int C::*>>::value);
}

// 23.15.4.3, type properties

// template <class T> struct is_const;
TEST_CASE("is_const", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_const<int const>>::value);
}

// template <class T> struct is_volatile;
TEST_CASE("is_volatile", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_volatile<int volatile>>::value);
}

// template <class T> struct is_trivial;
TEST_CASE("is_trivial", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_trivial<int>>::value);
}

#if SLB_TRIVIALITY_TRAITS
// template <class T> struct is_trivially_copyable;
TEST_CASE("is_trivially_copyable", "[meta.unary.prop]") {
  CHECK(
      std::is_base_of<slb::true_type, slb::is_trivially_copyable<int>>::value);
}
#endif

// template <class T> struct is_standard_layout;
TEST_CASE("is_standard_layout", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_standard_layout<int>>::value);
}

// template <class T> struct is_empty;
TEST_CASE("is_empty", "[meta.unary.prop]") {
  struct Empty {};
  CHECK(std::is_base_of<slb::true_type, slb::is_empty<Empty>>::value);
}

// template <class T> struct is_polymorphic;
TEST_CASE("is_polymorphic", "[meta.unary.prop]") {
  class Polymorphic {
    virtual void fun() {}
  };
  CHECK(
      std::is_base_of<slb::true_type, slb::is_polymorphic<Polymorphic>>::value);
}

// template <class T> struct is_abstract;
TEST_CASE("is_abstract", "[meta.unary.prop]") {
  class Abstract {
    virtual void fun() = 0;
  };
  CHECK(std::is_base_of<slb::true_type, slb::is_abstract<Abstract>>::value);
}

#if __cpp_lib_is_final || __has_feature(is_final) || (__GNUC__ > 4) ||         \
    ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 7))
// template <class T> struct is_final;
TEST_CASE("is_final", "[meta.unary.prop]") {
  class Final final {};
  CHECK(std::is_base_of<slb::true_type, slb::is_final<Final>>::value);
}
#endif

#if __cpp_lib_is_aggregate || __has_feature(is_aggregate) || (__GNUC__ >= 7)
// template <class T> struct is_aggregate;
TEST_CASE("is_aggregate", "[meta.unary.prop]") {
  struct Aggregate {
    int obj;
  };
  CHECK(std::is_base_of<slb::true_type, slb::is_aggregate<Aggregate>>::value);
}
#endif

// template <class T> struct is_signed;
TEST_CASE("is_signed", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_signed<signed int>>::value);
}

// template <class T> struct is_unsigned;
TEST_CASE("is_unsigned", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_unsigned<unsigned int>>::value);
}

// template <class T, class... Args> struct is_constructible;
TEST_CASE("is_constructible", "[meta.unary.prop]") {
  CHECK(
      std::is_base_of<slb::true_type, slb::is_constructible<int, int>>::value);
}

// template <class T> struct is_default_constructible;
TEST_CASE("is_default_constructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_default_constructible<int>>::value);
}

// template <class T> struct is_copy_constructible;
TEST_CASE("is_copy_constructible", "[meta.unary.prop]") {
  CHECK(
      std::is_base_of<slb::true_type, slb::is_copy_constructible<int>>::value);
}

// template <class T> struct is_move_constructible;
TEST_CASE("is_move_constructible", "[meta.unary.prop]") {
  CHECK(
      std::is_base_of<slb::true_type, slb::is_move_constructible<int>>::value);
}

#if SLB_TRIVIALITY_TRAITS
// template <class T, class ...Args> struct is_trivially_constructible;
TEST_CASE("is_trivially_constructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_trivially_constructible<int>>::value);
}

// template <class T> struct is_trivially_default_constructible;
TEST_CASE("is_trivially_default_constructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_trivially_default_constructible<int>>::value);
}

// template <class T> struct is_trivially_copy_constructible;
TEST_CASE("is_trivially_copy_constructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_trivially_copy_constructible<int>>::value);
}

// template <class T> struct is_trivially_move_constructible;
TEST_CASE("is_trivially_move_constructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_trivially_move_constructible<int>>::value);
}
#endif

// template <class T, class ...Args> struct is_nothrow_constructible;
TEST_CASE("is_nothrow_constructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_nothrow_constructible<int, int>>::value);
}

// template <class T> struct is_nothrow_default_constructible;
TEST_CASE("is_nothrow_default_constructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_nothrow_default_constructible<int>>::value);
}

// template <class T> struct is_nothrow_copy_constructible;
TEST_CASE("is_nothrow_copy_constructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_nothrow_copy_constructible<int>>::value);
}

// template <class T> struct is_nothrow_move_constructible;
TEST_CASE("is_nothrow_move_constructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_nothrow_move_constructible<int>>::value);
}

// template <class T> struct is_destructible;
TEST_CASE("is_destructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_destructible<int>>::value);
}

// template <class T> struct is_trivially_destructible;
TEST_CASE("is_trivially_destructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_trivially_destructible<int>>::value);
}

// template <class T> struct is_nothrow_destructible;
TEST_CASE("is_nothrow_destructible", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_nothrow_destructible<int>>::value);
}

// template <class T, class U> struct is_assignable;
TEST_CASE("is_assignable", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_assignable<int&, int>>::value);
}

// template <class T> struct is_copy_assignable;
TEST_CASE("is_copy_assignable", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_copy_assignable<int>>::value);
}

// template <class T> struct is_move_assignable;
TEST_CASE("is_move_assignable", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_move_assignable<int>>::value);
}

#if SLB_TRIVIALITY_TRAITS
// template <class T, class U> struct is_trivially_assignable;
TEST_CASE("is_trivially_assignable", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_trivially_assignable<int&, int>>::value);
}

// template <class T> struct is_trivially_copy_assignable;
TEST_CASE("is_trivially_copy_assignable", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_trivially_copy_assignable<int>>::value);
}

// template <class T> struct is_trivially_move_assignable;
TEST_CASE("is_trivially_move_assignable", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_trivially_move_assignable<int>>::value);
}
#endif

// template <class T, class U> struct is_nothrow_assignable;
TEST_CASE("is_nothrow_assignable", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_nothrow_assignable<int&, int>>::value);
}

// template <class T> struct is_nothrow_copy_assignable;
TEST_CASE("is_nothrow_copy_assignable", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_nothrow_copy_assignable<int>>::value);
}

// template <class T> struct is_nothrow_move_assignable;
TEST_CASE("is_nothrow_move_assignable", "[meta.unary.prop]") {
  CHECK(std::is_base_of<slb::true_type,
                        slb::is_nothrow_move_assignable<int>>::value);
}

// template <class T> struct has_virtual_destructor;
TEST_CASE("has_virtual_destructor", "[meta.unary.prop]") {
  class WithVirtualDestructor {
    virtual ~WithVirtualDestructor() {}
  };
  CHECK(std::is_base_of<
        slb::true_type,
        slb::has_virtual_destructor<WithVirtualDestructor>>::value);
}

#if __cpp_lib_has_unique_object_representations ||                             \
    __has_feature(has_unique_object_representations) || (__GNUC__ >= 7)
// template <class T> struct has_unique_object_representations;
TEST_CASE("has_unique_object_representations", "[meta.unary.prop]") {
  class WithUniqueObjectRepresentations {
    int obj;
  };
  CHECK(std::is_base_of<slb::true_type,
                        slb::has_unique_object_representations<
                            WithUniqueObjectRepresentations>>::value);
}
#endif

// 23.15.5, type property queries

// template <class T> struct alignment_of;
TEST_CASE("alignment_of", "[meta.unary.prop.query]") {
  CHECK(std::is_base_of<slb::integral_constant<std::size_t, alignof(int)>,
                        slb::alignment_of<int>>::value);
}

// template <class T> struct rank;
TEST_CASE("rank", "[meta.unary.prop.query]") {
  CHECK(std::is_base_of<slb::integral_constant<std::size_t, 3>,
                        slb::rank<int[2][2][2]>>::value);
}

// template <class T, unsigned I = 0> struct extent;
TEST_CASE("extent", "[meta.unary.prop.query]") {
  CHECK(std::is_base_of<slb::integral_constant<std::size_t, 2>,
                        slb::extent<int[2][3]>>::value);

  CHECK(std::is_base_of<slb::integral_constant<std::size_t, 3>,
                        slb::extent<int[2][3], 1>>::value);
}

// 23.15.6, type relations

// template <class T, class U> struct is_same;
TEST_CASE("is_same", "[meta.rel]") {
  CHECK(std::is_base_of<slb::true_type, slb::is_same<int, int>>::value);
}

// template <class Base, class Derived> struct is_base_of;
TEST_CASE("is_base_of", "[meta.rel]") {
  class Base {};
  class Derived : public Base {};
  CHECK(std::is_base_of<slb::true_type, slb::is_base_of<Base, Derived>>::value);
}

// template <class From, class To> struct is_convertible;
TEST_CASE("is_convertible", "[meta.rel]") {
  CHECK(
      std::is_base_of<slb::true_type, slb::is_convertible<float, int>>::value);
}

// 23.15.7.1, const-volatile modifications

// template <class T>
// using remove_const_t = typename remove_const<T>::type;
TEST_CASE("remove_const_t", "[meta.trans.cv]") {
  CHECK(std::is_same<slb::remove_const_t<int const>,
                     std::remove_const<int const>::type>::value);
}

// template <class T>
// using remove_volatile_t = typename remove_volatile<T>::type;
TEST_CASE("remove_volatile_t", "[meta.trans.cv]") {
  CHECK(std::is_same<slb::remove_volatile_t<int volatile>,
                     std::remove_volatile<int volatile>::type>::value);
}

// template <class T>
// using remove_cv_t = typename remove_cv<T>::type;
TEST_CASE("remove_cv_t", "[meta.trans.cv]") {
  CHECK(std::is_same<slb::remove_cv_t<int const volatile>,
                     std::remove_cv<int const volatile>::type>::value);
}

// template <class T>
// using add_const_t = typename add_const<T>::type;
TEST_CASE("add_const_t", "[meta.trans.cv]") {
  CHECK(std::is_same<slb::add_const_t<int>, std::add_const<int>::type>::value);
}

// template <class T>
// using add_volatile_t = typename add_volatile<T>::type;
TEST_CASE("add_volatile_t", "[meta.trans.cv]") {
  CHECK(std::is_same<slb::add_volatile_t<int>,
                     std::add_volatile<int>::type>::value);
}

// template <class T>
// using add_cv_t = typename add_cv<T>::type;
TEST_CASE("add_cv_t", "[meta.trans.cv]") {
  CHECK(std::is_same<slb::add_cv_t<int>, std::add_cv<int>::type>::value);
}

// 23.15.7.2, reference modifications

// template <class T>
// using remove_reference_t = typename remove_reference<T>::type;
TEST_CASE("remove_reference_t", "[meta.trans.ref]") {
  CHECK(std::is_same<slb::remove_reference_t<int&>,
                     std::remove_reference<int&>::type>::value);
}

// template <class T>
// using add_lvalue_reference_t = typename add_lvalue_reference<T>::type;
TEST_CASE("add_lvalue_reference_t", "[meta.trans.ref]") {
  CHECK(std::is_same<slb::add_lvalue_reference_t<int>,
                     std::add_lvalue_reference<int>::type>::value);
}

// template <class T>
// using add_rvalue_reference_t = typename add_rvalue_reference<T>::type;
TEST_CASE("add_rvalue_reference_t", "[meta.trans.ref]") {
  CHECK(std::is_same<slb::add_rvalue_reference_t<int>,
                     std::add_rvalue_reference<int>::type>::value);
}

// 23.15.7.3, sign modifications

// template <class T>
// using make_signed_t = typename make_signed<T>::type;
TEST_CASE("make_signed_t", "[meta.trans.sign]") {
  CHECK(std::is_same<slb::make_signed_t<int>,
                     std::make_signed<int>::type>::value);
}

// template <class T>
// using make_unsigned_t = typename make_unsigned<T>::type;
TEST_CASE("make_unsigned_t", "[meta.trans.sign]") {
  CHECK(std::is_same<slb::make_unsigned_t<int>,
                     std::make_unsigned<int>::type>::value);
}

// 23.15.7.4, array modifications

// template <class T>
// using remove_extent_t = typename remove_extent<T>::type;
TEST_CASE("remove_extent_t", "[meta.trans.arr]") {
  CHECK(std::is_same<slb::remove_extent_t<int[3]>,
                     std::remove_extent<int[3]>::type>::value);
}

// template <class T>
// using remove_all_extents_t = typename remove_all_extent<T>::type;
TEST_CASE("remove_all_extents_t", "[meta.trans.arr]") {
  CHECK(std::is_same<slb::remove_all_extents_t<int[3][2][1]>,
                     std::remove_all_extents<int[3][2][1]>::type>::value);
}

// 23.15.7.5, pointer modifications

// template <class T>
// using remove_pointer_t = typename remove_pointer<T>::type;
TEST_CASE("remove_pointer_t", "[meta.trans.ptr]") {
  CHECK(std::is_same<slb::remove_pointer_t<int*>,
                     std::remove_pointer<int*>::type>::value);
}

// template <class T>
// using add_pointer_t = typename add_pointer<T>::type;
TEST_CASE("add_pointer_t", "[meta.trans.ptr]") {
  CHECK(std::is_same<slb::add_pointer_t<int>,
                     std::add_pointer<int>::type>::value);
}

// 23.15.7.6, other transformations

// template <size_t Len, class... Types> struct aligned_union;
TEST_CASE("aligned_union", "[meta.trans.other]") {
  union U {
    int x;
    double y;
  };

  // The member typedef `type` shall be a trivial standard-layout type suitable
  // for use as uninitialized storage for any object whose type is listed in
  // `Types`; its size shall be at least `Len`.
  {
    using type = slb::aligned_union<0, int, double>::type;
    CHECK(std::is_trivial<type>::value);
    CHECK(std::is_standard_layout<type>::value);
    CHECK(alignof(type) % alignof(U) == 0);

    CHECK(sizeof(slb::aligned_union<sizeof(int) * 2, int>::type) >=
          sizeof(int) * 2);
  }

  // The static member `alignment_value` shall be an integral constant of type
  // `size_t` whose value is the strictest alignment of all types listed in
  // `Types`.
  {
    using au0 = slb::aligned_union<0, int, double>;
    CHECK(
        std::is_same<decltype(au0::alignment_value), std::size_t const>::value);
    CHECK(au0::alignment_value == alignof(U));
    constexpr std::size_t au0_alignment_value = au0::alignment_value;
    (void)au0_alignment_value;
  }
}

// template <size_t Len, size_t Align = default-alignment>
// using aligned_storage_t = typename aligned_storage<Len, Align>::type;
TEST_CASE("aligned_storage_t", "[meta.trans.other]") {
  CHECK(std::is_same<slb::aligned_storage_t<sizeof(int)>,
                     std::aligned_storage<sizeof(int)>::type>::value);

  CHECK(std::is_same<
        slb::aligned_storage_t<sizeof(int), alignof(int)>,
        std::aligned_storage<sizeof(int), alignof(int)>::type>::value);
}

// template <size_t Len, class... Types>
// using aligned_union_t = typename aligned_union<Len, Types...>::type;
TEST_CASE("aligned_union_t", "[meta.trans.other]") {
  CHECK(std::is_same<slb::aligned_union_t<1, int, float, double>,
                     slb::aligned_union<1, int, float, double>::type>::value);
}

// template <class T>
// using decay_t = typename decay<T>::type;
TEST_CASE("decay_t", "[meta.trans.other]") {
  CHECK(std::is_same<slb::decay_t<int const&>,
                     std::decay<int const&>::type>::value);
}

// template <bool B, typename T = void>
// using enable_if_t = typename enable_if<B, T>::type;
TEST_CASE("enable_if_t", "[meta.trans.other]") {
  CHECK(std::is_same<slb::enable_if_t<true, int>,
                     std::enable_if<true, int>::type>::value);
}

// template <bool B, typename T, typename F>
// using conditional_t = typename conditional<B, T, F>::type;
TEST_CASE("conditional_t", "[meta.trans.other]") {
  CHECK(std::is_same<slb::conditional_t<true, int, float>,
                     std::conditional<true, int, float>::type>::value);
}

// template <class... Ts>
// using common_type_t = typename common_type<Ts...>::type;
TEST_CASE("common_type_t", "[meta.trans.other]") {
  CHECK(std::is_same<slb::common_type_t<int, float, double>,
                     std::common_type<int, float, double>::type>::value);
}

// template <class T>
// using underlying_type_t = typename underlying_type<T>::type;
TEST_CASE("underlying_type_t", "[meta.trans.other]") {
  enum E : int {};
  CHECK(std::is_same<slb::underlying_type_t<E>,
                     std::underlying_type<E>::type>::value);
}

// template <class...>
// using void_t = void;

template <typename T, typename Enable = void>
struct has_nested_type : std::false_type {};

template <typename T>
struct has_nested_type<T, slb::void_t<typename T::type>> : std::true_type {};

TEST_CASE("void_t", "[meta.trans.other]") {
  CHECK(std::is_same<slb::void_t<int, float, double>, void>::value);

  /* sfinae */ {
    struct S {
      using type = int;
    };
    CHECK(has_nested_type<S>::value);
    CHECK_FALSE(has_nested_type<int>::value);
  }
}

// 23.15.8, logical operator traits

template <bool B>
struct Weird {
  struct Value {
    operator int() const { return !B; }
    constexpr explicit operator bool() const { return B; }
  };

  static constexpr Value value{};
};

template <bool B>
constexpr typename Weird<B>::Value Weird<B>::value;

// template<class... B> struct conjunction;
TEST_CASE("conjunction", "[meta.logical]") {
  /* length: 0 */ {
    CHECK(std::is_base_of<slb::true_type, slb::conjunction<>>::value);
  }

  /* length: 1 */ {
    CHECK(std::is_base_of<slb::true_type,
                          slb::conjunction<slb::true_type>>::value);

    CHECK(std::is_base_of<slb::false_type,
                          slb::conjunction<slb::false_type>>::value);

    CHECK(std::is_base_of<Weird<true>, slb::conjunction<Weird<true>>>::value);
    CHECK(std::is_base_of<Weird<false>, slb::conjunction<Weird<false>>>::value);
  }

  /* length: 2 */ {
    CHECK(std::is_base_of<
          slb::true_type,
          slb::conjunction<slb::true_type, slb::true_type>>::value);

    CHECK(std::is_base_of<
          slb::false_type,
          slb::conjunction<slb::true_type, slb::false_type>>::value);

    CHECK(std::is_base_of<
          slb::false_type,
          slb::conjunction<slb::false_type, slb::true_type>>::value);

    CHECK(std::is_base_of<
          slb::false_type,
          slb::conjunction<slb::false_type, slb::false_type>>::value);

    CHECK(std::is_base_of<Weird<true>,
                          slb::conjunction<Weird<true>, Weird<true>>>::value);

    CHECK(std::is_base_of<Weird<false>,
                          slb::conjunction<Weird<true>, Weird<false>>>::value);

    CHECK(std::is_base_of<Weird<false>,
                          slb::conjunction<Weird<false>, Weird<true>>>::value);

    CHECK(std::is_base_of<Weird<false>,
                          slb::conjunction<Weird<false>, Weird<false>>>::value);
  }

  /* short-circuiting */ {
    CHECK(std::is_base_of<slb::false_type,
                          slb::conjunction<slb::false_type, void>>::value);

    CHECK(std::is_base_of<
          Weird<false>,
          slb::conjunction<Weird<true>, Weird<false>, void>>::value);
  }
}

// template<class... B> struct disjunction;
TEST_CASE("disjunction", "[meta.logical]") {
  /* length: 0 */ {
    CHECK(std::is_base_of<slb::false_type, slb::disjunction<>>::value);
  }

  /* length: 1 */ {
    CHECK(std::is_base_of<std::true_type,
                          slb::disjunction<slb::true_type>>::value);

    CHECK(std::is_base_of<std::false_type,
                          slb::disjunction<slb::false_type>>::value);

    CHECK(std::is_base_of<Weird<true>, slb::disjunction<Weird<true>>>::value);
    CHECK(std::is_base_of<Weird<false>, slb::disjunction<Weird<false>>>::value);
  }

  /* length: 2 */ {
    CHECK(std::is_base_of<
          slb::true_type,
          slb::disjunction<slb::true_type, slb::true_type>>::value);

    CHECK(std::is_base_of<
          slb::true_type,
          slb::disjunction<slb::true_type, slb::false_type>>::value);

    CHECK(std::is_base_of<
          slb::true_type,
          slb::disjunction<slb::false_type, slb::true_type>>::value);

    CHECK(std::is_base_of<
          slb::false_type,
          slb::disjunction<slb::false_type, slb::false_type>>::value);

    CHECK(std::is_base_of<Weird<true>,
                          slb::disjunction<Weird<true>, Weird<true>>>::value);

    CHECK(std::is_base_of<Weird<true>,
                          slb::disjunction<Weird<true>, Weird<false>>>::value);

    CHECK(std::is_base_of<Weird<true>,
                          slb::disjunction<Weird<false>, Weird<true>>>::value);

    CHECK(std::is_base_of<Weird<false>,
                          slb::disjunction<Weird<false>, Weird<false>>>::value);
  }

  /* short-circuiting */ {
    CHECK(std::is_base_of<slb::true_type,
                          slb::disjunction<slb::true_type, void>>::value);

    CHECK(std::is_base_of<
          Weird<true>,
          slb::disjunction<Weird<false>, Weird<true>, void>>::value);
  }
}

// template<class B> struct negation;
TEST_CASE("negation", "[meta.logical]") {
  CHECK(std::is_base_of<slb::false_type, slb::negation<slb::true_type>>::value);
  CHECK(std::is_base_of<slb::true_type, slb::negation<slb::false_type>>::value);

  CHECK(std::is_base_of<slb::true_type,
                        slb::negation<slb::negation<Weird<true>>>>::value);
  CHECK(std::is_base_of<slb::false_type,
                        slb::negation<slb::negation<Weird<false>>>>::value);
}

// 23.15.9, endian

// enum class endian {
//   little = see below,
//   big = see below,
//   native = see below
// };
TEST_CASE("endian", "[meta.endian]") {
  CHECK(slb::endian::little != slb::endian::big);

#if defined(_WIN32) || (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
  CHECK(slb::endian::native == slb::endian::little);
#elif (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
  CHECK(slb::endian::native == slb::endian::big);
#else
  CHECK(slb::endian::native != slb::endian::little);
  CHECK(slb::endian::native != slb::endian::big);
#endif
}
