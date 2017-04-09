//===- unittests/BuildSystem/BuildValueTest.cpp ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llbuild/BuildSystem/BuildValue.h"

#include "gtest/gtest.h"

using namespace llbuild;
using namespace llbuild::buildsystem;
using namespace llvm;

namespace {

/// We should support decoding an empty value without crashing.
TEST(BuildValueTest, emptyDecode) {
  auto result = BuildValue::fromData(core::ValueType());
  EXPECT_EQ(result.toData(), BuildValue::makeInvalid().toData());
}

TEST(BuildValueTest, virtualValueSerialization) {
  // Check that two identical values are equivalent.
  {
    BuildValue a = BuildValue::makeVirtualInput();
    EXPECT_EQ(a.toData(), BuildValue::makeVirtualInput().toData());
  }

  // Check that a moved complex value is equivalent.
  {
    BuildValue tmp = BuildValue::makeVirtualInput();
    BuildValue a = std::move(tmp);
    EXPECT_EQ(a.toData(), BuildValue::makeVirtualInput().toData());
  }

  // Check that an rvalue initialized complex value is equivalent.
  {
    BuildValue tmp = BuildValue::makeVirtualInput();
    BuildValue a{ std::move(tmp) };
    EXPECT_EQ(a.toData(), BuildValue::makeVirtualInput().toData());
  }

  // Check that a round-tripped value is equivalent.
  EXPECT_EQ(BuildValue::makeVirtualInput().toData(),
            BuildValue::fromData(
                BuildValue::makeVirtualInput().toData()).toData());
}

TEST(BuildValueTest, commandValueSingleOutputSerialization) {
  uint64_t signature = 0xDEADBEEF;
  basic::FileInfo infos[1] = {};
  infos[0].size = 1;
  BuildValue outputs[1] = { BuildValue::makeExistingInput(infos[0]) };
  
  // Check that two identical values are equivalent.
  {
    BuildValue a = BuildValue::makeSuccessfulCommand(signature, outputs);
    EXPECT_EQ(a.toData(),
              BuildValue::makeSuccessfulCommand(signature, outputs).toData());
  }

  // Check that a moved complex value is equivalent.
  {
    BuildValue tmp = BuildValue::makeSuccessfulCommand(signature, outputs);
    BuildValue a = std::move(tmp);
    EXPECT_EQ(a.toData(),
              BuildValue::makeSuccessfulCommand(signature, outputs).toData());
  }

  // Check that an rvalue initialized complex value is equivalent.
  {
    BuildValue tmp = BuildValue::makeSuccessfulCommand(signature, outputs);
    BuildValue a{ std::move(tmp) };
    EXPECT_EQ(a.toData(),
              BuildValue::makeSuccessfulCommand(signature, outputs).toData());
  }

  // Check that a round-tripped value is equivalent.
  EXPECT_EQ(BuildValue::makeSuccessfulCommand(signature, outputs).toData(),
            BuildValue::fromData(
                BuildValue::makeSuccessfulCommand(
                    signature, outputs).toData()).toData());
}

TEST(BuildValueTest, commandValueMultipleOutputsSerialization) {
  uint64_t signature = 0xDEADBEEF;
  basic::FileInfo infos[2] = {};
  infos[0].size = 1;
  infos[1].size = 2;
  BuildValue outputs[2] = { BuildValue::makeExistingInput(infos[0]),
                            BuildValue::makeExistingInput(infos[1]) };
  
  // Check that two identical values are equivalent.
  {
    BuildValue a = BuildValue::makeSuccessfulCommand(signature, outputs);
    EXPECT_EQ(a.toData(),
              BuildValue::makeSuccessfulCommand(signature, outputs).toData());
  }

  // Check that a moved complex value is equivalent.
  {
    BuildValue tmp = BuildValue::makeSuccessfulCommand(signature, outputs);
    BuildValue a = std::move(tmp);
    EXPECT_EQ(a.toData(),
              BuildValue::makeSuccessfulCommand(signature, outputs).toData());
  }

  // Check that an rvalue initialized complex value is equivalent.
  {
    BuildValue tmp = BuildValue::makeSuccessfulCommand(signature, outputs);
    BuildValue a{ std::move(tmp) };
    EXPECT_EQ(a.toData(),
              BuildValue::makeSuccessfulCommand(signature, outputs).toData());
  }

  // Check that a round-tripped value is equivalent.
  EXPECT_EQ(BuildValue::makeSuccessfulCommand(signature, outputs).toData(),
            BuildValue::fromData(
                BuildValue::makeSuccessfulCommand(
                    signature, outputs).toData()).toData());
}

TEST(BuildValueTest, directoryListValues) {
  basic::FileInfo mockInfo{};
  std::vector<std::string> strings{ "hello", "world" };
  
  // Check that two identical values are equivalent.
  {
    BuildValue a = BuildValue::makeDirectoryContents(mockInfo, strings);
    EXPECT_EQ(a.toData(),
              BuildValue::makeDirectoryContents(mockInfo, strings).toData());
  }

  // Check that a moved complex value is equivalent.
  {
    BuildValue tmp = BuildValue::makeDirectoryContents(mockInfo, strings);
    BuildValue a = std::move(tmp);
    EXPECT_EQ(a.toData(),
              BuildValue::makeDirectoryContents(mockInfo, strings).toData());
  }

  // Check that an rvalue initialized complex value is equivalent.
  {
    BuildValue tmp = BuildValue::makeDirectoryContents(mockInfo, strings);
    BuildValue a{ std::move(tmp) };
    EXPECT_EQ(a.toData(),
              BuildValue::makeDirectoryContents(mockInfo, strings).toData());
  }

  // Check the contents are correct.
  {
    BuildValue tmp = BuildValue::makeDirectoryContents(mockInfo, strings);
    auto result = tmp.getDirectoryContents();
    EXPECT_EQ(result.size(), 2U);
    EXPECT_EQ(result[0], "hello");
    EXPECT_EQ(result[1], "world");
  }
}

}
