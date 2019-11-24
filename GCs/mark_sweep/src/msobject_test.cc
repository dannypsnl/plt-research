#include <gtest/gtest.h>

#include "msobject.hpp"

TEST(MSObject, Mark) {
  auto obj = MSObject("test");
  ASSERT_EQ(obj.is_marked, false);
  mark(&obj);
  ASSERT_EQ(obj.is_marked, true);
}
