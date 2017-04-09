//===- BuildValue.h ---------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLBUILD_BUILDSYSTEM_BUILDVALUE_H
#define LLBUILD_BUILDSYSTEM_BUILDVALUE_H

#include "llbuild/Core/BuildEngine.h"
#include "llbuild/Basic/BinaryCoding.h"
#include "llbuild/Basic/Compiler.h"
#include "llbuild/Basic/FileInfo.h"
#include "llbuild/Basic/LLVM.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

#include <vector>

namespace llvm {
class raw_ostream;
}

namespace llbuild {
namespace buildsystem {

/// The BuildValue encodes the value space used by the BuildSystem when using
/// the core BuildEngine.
class BuildValue {
  using FileInfo = basic::FileInfo;

  enum class Kind : uint32_t {
    /// An invalid value, for sentinel purposes.
    Invalid = 0,

    /// A value produced by a virtual input.
    VirtualInput,

    /// A value produced by an existing input file.
    ExistingInput,

    /// A value produced by an existing symbolic link.
    ExistingLink,

    /// A value produced by a missing input file.
    MissingInput,

    /// The contents of a directory.
    DirectoryContents,

    /// The signature of a directories contents.
    DirectoryTreeSignature,

    /// A value produced by a command which succeeded, but whose output was
    /// missing.
    MissingOutput,

    /// A value for a produced output whose command failed or was cancelled.
    FailedInput,

    /// A value produced by a successful command.
    SuccessfulCommand,

    /// A value produced by a failing command.
    FailedCommand,

    /// A value produced by a command which was skipped because one of its
    /// dependencies failed.
    PropagatedFailureCommand,

    /// A value produced by a command which was cancelled.
    CancelledCommand,

    /// A value produced by a command which was skipped.
    SkippedCommand,

    /// Sentinel value representing the result of "building" a top-level target.
    Target,
  };

  /// Get the name of the given kind.
  static StringRef stringForKind(Kind);

  friend struct basic::BinaryCodingTraits<BuildValue::Kind>;
  friend struct basic::BinaryCodingTraits<BuildValue>;

  /// The kind of value.
  Kind kind = Kind::Invalid;

  /// The payload for the specific value kind, if any.
  union {
    /// The payload of an existing input file.
    struct {
      /// The information describing the file.
      FileInfo fileInfo;
    } asExistingInput;

    /// The payload of an existing input link.
    struct {
      /// The information describing the file.
      FileInfo fileInfo;

      /// The link target value.
      char* target;
    } asExistingLink;

    /// The payload of directory contents.
    struct {
      /// The information describing the directory.
      FileInfo fileInfo;

      /// The names of each member of the directory, packed as a sequence of C
      /// strings.
      char* contents;

      /// The total length of the contents.
      uint64_t contentsSize;
    } asDirectoryContents;

    /// The payload of a directory tree signature.
    struct {
      /// The signature of the command.
      uint64_t signature;
    } asDirectoryTreeSignature;

    /// The payload of a successful command.
    struct {
      /// The signature of the command.
      uint64_t signature;

      /// The number of outputs.
      uint32_t numOutputs;

      /// The values for each output, respectively.
      BuildValue* outputs;
    } asSuccessfulCommand;
  } valueData;
  
private:
  BuildValue() : kind(Kind::Invalid) {}
  BuildValue(Kind kind) : kind(kind) {}
  BuildValue(basic::BinaryDecoder& decoder);
  BuildValue(Kind kind, FileInfo fileInfo) : kind(kind) {
    assert(kind == Kind::ExistingInput);
    valueData.asExistingInput.fileInfo = fileInfo;
  }
  BuildValue(Kind kind, FileInfo fileInfo, StringRef target) : kind(kind) {
    assert(kind == Kind::ExistingLink);
    valueData.asExistingLink.fileInfo = fileInfo;
    valueData.asExistingLink.target = new char[target.size() + 1];
    memcpy(valueData.asExistingLink.target, target.data(), target.size());
    valueData.asExistingLink.target[target.size()] = '\0';
  }
  BuildValue(Kind kind, uint64_t signature) : kind(kind) {
    assert(kind == Kind::DirectoryTreeSignature);
    valueData.asDirectoryTreeSignature.signature = signature;
  }
  BuildValue(Kind kind, uint64_t signature, ArrayRef<BuildValue> outputs)
      : kind(kind)
  {
    assert(kind == Kind::SuccessfulCommand);
    valueData.asSuccessfulCommand.signature = signature;
    valueData.asSuccessfulCommand.numOutputs = outputs.size();
    valueData.asSuccessfulCommand.outputs = new BuildValue[outputs.size()];
    for (uint32_t i = 0; i != outputs.size(); ++i) {
      // FIXME: It is unfortunate we end up doing a lot of copies here in
      // practice, we really want to move in the entire array of outputs.
      valueData.asSuccessfulCommand.outputs[i] = outputs[i];
    }
  }
  
  /// Create a build value containing file information and string values.
  BuildValue(Kind kind, FileInfo fileInfo, ArrayRef<std::string> values)
      : kind(kind)
  {
    assert(kind == Kind::DirectoryContents);
    valueData.asDirectoryContents.fileInfo = fileInfo;
    // Construct the concatenated data.
    uint64_t size = 0;
    for (auto value: values) {
      size += value.size() + 1;
    }
    char *p, *contents = p = new char[size];
    for (auto value: values) {
      assert(value.find('\0') == StringRef::npos);
      memcpy(p, value.data(), value.size());
      p += value.size();
      *p++ = '\0';
    }
    valueData.asDirectoryContents.contents = contents;
    valueData.asDirectoryContents.contentsSize = size;
  }

  /// Encode the value onto the given coder.
  void encode(basic::BinaryEncoder& coder) const;
  
public:
  BuildValue(BuildValue&& rhs) : kind(rhs.kind) {
    switch (kind) {
    case Kind::Invalid:
    case Kind::VirtualInput:
    case Kind::MissingInput:
    case Kind::MissingOutput:
    case Kind::FailedInput:
    case Kind::FailedCommand:
    case Kind::PropagatedFailureCommand:
    case Kind::CancelledCommand:
    case Kind::SkippedCommand:
    case Kind::Target:
      break;
      
    case Kind::ExistingInput:
      valueData.asExistingInput.fileInfo =
        rhs.valueData.asExistingInput.fileInfo;
      break;
    case Kind::ExistingLink:
      valueData.asExistingLink.fileInfo =
        rhs.valueData.asExistingLink.fileInfo;
      valueData.asExistingLink.target = rhs.valueData.asExistingLink.target;
      break;
    case Kind::DirectoryContents:
      valueData.asDirectoryContents.fileInfo =
        rhs.valueData.asDirectoryContents.fileInfo;
      valueData.asDirectoryContents.contents =
        rhs.valueData.asDirectoryContents.contents;
      valueData.asDirectoryContents.contentsSize =
        rhs.valueData.asDirectoryContents.contentsSize;
      break;
    case Kind::DirectoryTreeSignature:
      valueData.asDirectoryTreeSignature.signature =
        rhs.valueData.asDirectoryTreeSignature.signature;
      break;
    case Kind::SuccessfulCommand:
      valueData.asSuccessfulCommand.signature =
        rhs.valueData.asSuccessfulCommand.signature;
      valueData.asSuccessfulCommand.numOutputs =
        rhs.valueData.asSuccessfulCommand.numOutputs;
      valueData.asSuccessfulCommand.outputs =
        rhs.valueData.asSuccessfulCommand.outputs;
      break;
    }

    // Invalidate the RHS.
    rhs.kind = Kind::Invalid;
  }
  BuildValue(const BuildValue& rhs) : kind(rhs.kind) {
    switch (kind) {
    case Kind::Invalid:
    case Kind::VirtualInput:
    case Kind::MissingInput:
    case Kind::MissingOutput:
    case Kind::FailedInput:
    case Kind::FailedCommand:
    case Kind::PropagatedFailureCommand:
    case Kind::CancelledCommand:
    case Kind::SkippedCommand:
    case Kind::Target:
      break;
      
    case Kind::ExistingInput:
      valueData.asExistingInput.fileInfo =
        rhs.valueData.asExistingInput.fileInfo;
      break;
    case Kind::ExistingLink: {
      valueData.asExistingLink.fileInfo =
        rhs.valueData.asExistingLink.fileInfo;
      uint32_t targetSize = (uint32_t) ::strlen(
          rhs.valueData.asExistingLink.target);
      valueData.asExistingLink.target = new char[targetSize + 1];
      memcpy(valueData.asExistingLink.target,
             rhs.valueData.asExistingLink.target,
             targetSize + 1);
      break;
    }
    case Kind::DirectoryContents:
      valueData.asDirectoryContents.fileInfo =
        rhs.valueData.asDirectoryContents.fileInfo;
      valueData.asDirectoryContents.contentsSize =
        rhs.valueData.asDirectoryContents.contentsSize;
      valueData.asDirectoryContents.contents =
        new char[valueData.asDirectoryContents.contentsSize];
      memcpy(valueData.asDirectoryContents.contents,
             rhs.valueData.asDirectoryContents.contents,
             valueData.asDirectoryContents.contentsSize);
      break;
    case Kind::DirectoryTreeSignature:
      valueData.asDirectoryTreeSignature.signature =
        rhs.valueData.asDirectoryTreeSignature.signature;
      break;
    case Kind::SuccessfulCommand:
      valueData.asSuccessfulCommand.signature =
        rhs.valueData.asSuccessfulCommand.signature;
      valueData.asSuccessfulCommand.numOutputs =
        rhs.valueData.asSuccessfulCommand.numOutputs;
      valueData.asSuccessfulCommand.outputs = new BuildValue[
          valueData.asSuccessfulCommand.numOutputs];
      for (uint32_t i = 0; i != valueData.asSuccessfulCommand.numOutputs; ++i) {
        valueData.asSuccessfulCommand.outputs[i] =
          rhs.valueData.asSuccessfulCommand.outputs[i];
      }
      break;
    }
  }
  BuildValue& operator=(BuildValue&& rhs) {
    if (this != &rhs) {
      // Release existing data.
      switch (kind) {
      case Kind::Invalid:
      case Kind::VirtualInput:
      case Kind::MissingInput:
      case Kind::MissingOutput:
      case Kind::FailedInput:
      case Kind::FailedCommand:
      case Kind::PropagatedFailureCommand:
      case Kind::CancelledCommand:
      case Kind::SkippedCommand:
      case Kind::Target:
        break;

      case Kind::ExistingInput:
      case Kind::DirectoryTreeSignature:
        break;
        
      case Kind::ExistingLink:
        delete[] valueData.asExistingLink.target;
        break;
      case Kind::DirectoryContents:
        delete[] valueData.asDirectoryContents.contents;
        break;
      case Kind::SuccessfulCommand:
        delete[] valueData.asSuccessfulCommand.outputs;
        break;
      }

      // Move the data.
      kind = rhs.kind;
      switch (kind) {
      case Kind::Invalid:
      case Kind::VirtualInput:
      case Kind::MissingInput:
      case Kind::MissingOutput:
      case Kind::FailedInput:
      case Kind::FailedCommand:
      case Kind::PropagatedFailureCommand:
      case Kind::CancelledCommand:
      case Kind::SkippedCommand:
      case Kind::Target:
        break;
        
      case Kind::ExistingInput:
        valueData.asExistingInput.fileInfo =
          rhs.valueData.asExistingInput.fileInfo;
        break;
      case Kind::ExistingLink:
        valueData.asExistingLink.fileInfo =
          rhs.valueData.asExistingLink.fileInfo;
        valueData.asExistingLink.target = rhs.valueData.asExistingLink.target;
        break;
      case Kind::DirectoryContents:
        valueData.asDirectoryContents.fileInfo =
          rhs.valueData.asDirectoryContents.fileInfo;
        valueData.asDirectoryContents.contents =
          rhs.valueData.asDirectoryContents.contents;
        valueData.asDirectoryContents.contentsSize =
          rhs.valueData.asDirectoryContents.contentsSize;
        break;
      case Kind::DirectoryTreeSignature:
        valueData.asDirectoryTreeSignature.signature =
          rhs.valueData.asDirectoryTreeSignature.signature;
        break;
      case Kind::SuccessfulCommand:
        valueData.asSuccessfulCommand.signature =
          rhs.valueData.asSuccessfulCommand.signature;
        valueData.asSuccessfulCommand.numOutputs =
          rhs.valueData.asSuccessfulCommand.numOutputs;
        valueData.asSuccessfulCommand.outputs =
          rhs.valueData.asSuccessfulCommand.outputs;
        break;
      }
      // Invalidate the RHS.
      rhs.kind = Kind::Invalid;
    }
    return *this;
  }
  BuildValue& operator=(const BuildValue& rhs) {
    if (this != &rhs) {
      // Release existing data.
      switch (kind) {
      case Kind::Invalid:
      case Kind::VirtualInput:
      case Kind::MissingInput:
      case Kind::MissingOutput:
      case Kind::FailedInput:
      case Kind::FailedCommand:
      case Kind::PropagatedFailureCommand:
      case Kind::CancelledCommand:
      case Kind::SkippedCommand:
      case Kind::Target:
        break;

      case Kind::ExistingInput:
      case Kind::DirectoryTreeSignature:
        break;
        
      case Kind::ExistingLink:
        delete[] valueData.asExistingLink.target;
        break;
      case Kind::DirectoryContents:
        delete[] valueData.asDirectoryContents.contents;
        break;
      case Kind::SuccessfulCommand:
        delete[] valueData.asSuccessfulCommand.outputs;
        break;
      }

      kind = rhs.kind;
      switch (kind) {
      case Kind::Invalid:
      case Kind::VirtualInput:
      case Kind::MissingInput:
      case Kind::MissingOutput:
      case Kind::FailedInput:
      case Kind::FailedCommand:
      case Kind::PropagatedFailureCommand:
      case Kind::CancelledCommand:
      case Kind::SkippedCommand:
      case Kind::Target:
        break;
        
      case Kind::ExistingInput:
        valueData.asExistingInput.fileInfo =
          rhs.valueData.asExistingInput.fileInfo;
        break;
      case Kind::ExistingLink: {
        valueData.asExistingLink.fileInfo =
          rhs.valueData.asExistingLink.fileInfo;
        uint32_t targetSize = (uint32_t) ::strlen(
            rhs.valueData.asExistingLink.target);
        valueData.asExistingLink.target = new char[targetSize + 1];
        memcpy(valueData.asExistingLink.target,
               rhs.valueData.asExistingLink.target,
               targetSize + 1);
        break;
      }
      case Kind::DirectoryContents:
        valueData.asDirectoryContents.fileInfo =
          rhs.valueData.asDirectoryContents.fileInfo;
        valueData.asDirectoryContents.contentsSize =
          rhs.valueData.asDirectoryContents.contentsSize;
        valueData.asDirectoryContents.contents =
          new char[valueData.asDirectoryContents.contentsSize];
        memcpy(valueData.asDirectoryContents.contents,
               rhs.valueData.asDirectoryContents.contents,
               valueData.asDirectoryContents.contentsSize);
        break;
      case Kind::DirectoryTreeSignature:
        valueData.asDirectoryTreeSignature.signature =
          rhs.valueData.asDirectoryTreeSignature.signature;
        break;
      case Kind::SuccessfulCommand:
        valueData.asSuccessfulCommand.signature =
          rhs.valueData.asSuccessfulCommand.signature;
        valueData.asSuccessfulCommand.numOutputs =
          rhs.valueData.asSuccessfulCommand.numOutputs;
        valueData.asSuccessfulCommand.outputs = new BuildValue[
            valueData.asSuccessfulCommand.numOutputs];
        for (uint32_t i = 0; i !=
               valueData.asSuccessfulCommand.numOutputs; ++i) {
          valueData.asSuccessfulCommand.outputs[i] =
            rhs.valueData.asSuccessfulCommand.outputs[i];
        }
        break;
      }
    }
    return *this;
  }
  ~BuildValue() {
    switch (kind) {
    case Kind::Invalid:
    case Kind::VirtualInput:
    case Kind::MissingInput:
    case Kind::MissingOutput:
    case Kind::FailedInput:
    case Kind::FailedCommand:
    case Kind::PropagatedFailureCommand:
    case Kind::CancelledCommand:
    case Kind::SkippedCommand:
    case Kind::Target:
      break;

    case Kind::ExistingInput:
    case Kind::DirectoryTreeSignature:
      break;

    case Kind::ExistingLink:
      delete[] valueData.asExistingLink.target;
      break;
    case Kind::DirectoryContents:
      delete[] valueData.asDirectoryContents.contents;
      break;
    case Kind::SuccessfulCommand:
      delete[] valueData.asSuccessfulCommand.outputs;
      break;
    }
  }

  bool operator==(const BuildValue& rhs) const {
    if (kind != rhs.kind)
      return false;

    switch (kind) {
    case Kind::Invalid:
    case Kind::VirtualInput:
    case Kind::MissingInput:
    case Kind::MissingOutput:
    case Kind::FailedInput:
    case Kind::FailedCommand:
    case Kind::PropagatedFailureCommand:
    case Kind::CancelledCommand:
    case Kind::SkippedCommand:
    case Kind::Target:
      return true;

    case Kind::ExistingInput:
      return getExistingInputFileInfo() == rhs.getExistingInputFileInfo();
    case Kind::DirectoryTreeSignature:
      return getDirectoryTreeSignature() == rhs.getDirectoryTreeSignature();

    case Kind::ExistingLink:
      if (getExistingLinkFileInfo() != rhs.getExistingLinkFileInfo()) {
        return false;
      }
      return getExistingLinkTarget() == rhs.getExistingLinkTarget();
    case Kind::DirectoryContents:
      if (getDirectoryContentsFileInfo() !=
          rhs.getDirectoryContentsFileInfo()) {
        return false;
      }
      return getDirectoryContents() == rhs.getDirectoryContents();
    case Kind::SuccessfulCommand:
      if (getSuccessfulCommandSignature() !=
          rhs.getSuccessfulCommandSignature()) {
        return false;
      }
      if (getSuccessfulCommandNumOutputs() !=
          rhs.getSuccessfulCommandNumOutputs()) {
        return false;
      }
      for (unsigned i = 0, e = getSuccessfulCommandNumOutputs(); i != e; ++i) {
        if (getSuccessfulCommandOutput(i) != rhs.getSuccessfulCommandOutput(i))
          return false;
      }
      return true;
    }

    return false;
  }
  bool operator!=(const BuildValue& rhs) const {
    return !(*this == rhs);
  }
  
  /// @name Construction Functions
  /// @{

  static BuildValue makeInvalid() {
    return BuildValue(Kind::Invalid);
  }
  static BuildValue makeVirtualInput() {
    return BuildValue(Kind::VirtualInput);
  }
  static BuildValue makeExistingInput(FileInfo fileInfo) {
    assert(!fileInfo.isMissing());
    return BuildValue(Kind::ExistingInput, fileInfo);
  }
  static BuildValue makeExistingLink(FileInfo fileInfo,
                                     StringRef target) {
    return BuildValue(Kind::ExistingLink, fileInfo, target);
  }
  static BuildValue makeMissingInput() {
    return BuildValue(Kind::MissingInput);
  }
  static BuildValue makeDirectoryContents(FileInfo directoryInfo,
                                          ArrayRef<std::string> values) {
    return BuildValue(Kind::DirectoryContents, directoryInfo, values);
  }
  static BuildValue makeDirectoryTreeSignature(uint64_t signature) {
    return BuildValue(Kind::DirectoryTreeSignature, signature);
  }
  static BuildValue makeMissingOutput() {
    return BuildValue(Kind::MissingOutput);
  }
  static BuildValue makeFailedInput() {
    return BuildValue(Kind::FailedInput);
  }
  static BuildValue makeSuccessfulCommand(uint64_t commandSignature,
                                          ArrayRef<BuildValue> outputs) {
    return BuildValue(Kind::SuccessfulCommand, commandSignature, outputs);
  }
  static BuildValue makeFailedCommand() {
    return BuildValue(Kind::FailedCommand);
  }
  static BuildValue makePropagatedFailureCommand() {
    return BuildValue(Kind::PropagatedFailureCommand);
  }
  static BuildValue makeCancelledCommand() {
    return BuildValue(Kind::CancelledCommand);
  }
  static BuildValue makeSkippedCommand() {
    return BuildValue(Kind::SkippedCommand);
  }
  static BuildValue makeTarget() {
    return BuildValue(Kind::Target);
  }

  /// @}

  /// @name Accessors
  /// @{

  bool isInvalid() const { return kind == Kind::Invalid; }
  bool isVirtualInput() const { return kind == Kind::VirtualInput; }
  bool isExistingInput() const { return kind == Kind::ExistingInput; }
  bool isExistingLink() const { return kind == Kind::ExistingLink; }
  bool isMissingInput() const { return kind == Kind::MissingInput; }

  bool isDirectoryContents() const { return kind == Kind::DirectoryContents; }
  bool isDirectoryTreeSignature() const {
    return kind == Kind::DirectoryTreeSignature;
  }
  
  bool isMissingOutput() const { return kind == Kind::MissingOutput; }
  bool isFailedInput() const { return kind == Kind::FailedInput; }
  bool isSuccessfulCommand() const {return kind == Kind::SuccessfulCommand; }
  bool isFailedCommand() const { return kind == Kind::FailedCommand; }
  bool isPropagatedFailureCommand() const {
    return kind == Kind::PropagatedFailureCommand;
  }
  bool isCancelledCommand() const { return kind == Kind::CancelledCommand; }
  bool isSkippedCommand() const { return kind == Kind::SkippedCommand; }
  bool isTarget() const { return kind == Kind::Target; }

  const FileInfo& getExistingInputFileInfo() const {
    assert(isExistingInput() && "invalid call for value kind");
    return valueData.asExistingInput.fileInfo;
  }

  const FileInfo& getExistingLinkFileInfo() const {
    assert(isExistingLink() && "invalid call for value kind");
    return valueData.asExistingLink.fileInfo;
  }
  StringRef getExistingLinkTarget() const {
    assert(isExistingLink() && "invalid call for value kind");
    return valueData.asExistingLink.target;
  }

  const FileInfo& getDirectoryContentsFileInfo() const {
    assert(isDirectoryContents() && "invalid call for value kind");
    return valueData.asDirectoryContents.fileInfo;
  }

  std::vector<StringRef> getDirectoryContents() const {
    assert(isDirectoryContents() && "invalid call for value kind");

    // FIXME: It would be more efficient to simply provide an iterator based
    // interface.
    std::vector<StringRef> result;
    for (uint64_t i = 0; i < valueData.asDirectoryContents.contentsSize;) {
      auto value = StringRef(&valueData.asDirectoryContents.contents[i]);
      assert(i + value.size() <= valueData.asDirectoryContents.contentsSize);
      result.push_back(value);
      i += value.size() + 1;
    }
    return result;
  }
  
  uint64_t getDirectoryTreeSignature() const {
    assert(isDirectoryTreeSignature() && "invalid call for value kind");
    return valueData.asDirectoryTreeSignature.signature;
  }

  uint64_t getSuccessfulCommandSignature() const {
    assert(isSuccessfulCommand() && "invalid call for value kind");
    return valueData.asSuccessfulCommand.signature;
  }

  unsigned getSuccessfulCommandNumOutputs() const {
    assert(isSuccessfulCommand() && "invalid call for value kind");
    return valueData.asSuccessfulCommand.numOutputs;
  }

  const BuildValue& getSuccessfulCommandOutput(unsigned n) const {
    assert(isSuccessfulCommand() && "invalid call for value kind");
    assert(n < getSuccessfulCommandNumOutputs() && "invalid index");
    return valueData.asSuccessfulCommand.outputs[n];
  }

  /// @}

  /// @name Conversion to core ValueType.
  /// @{

  static BuildValue fromData(const core::ValueType& value) {
    basic::BinaryDecoder decoder(StringRef((char*)value.data(), value.size()));
    auto result = BuildValue(decoder);
    decoder.finish();
    return result;
  }
  core::ValueType toData() const;

  /// @}

  /// @name Debug Support
  /// @{

  void dump(raw_ostream& OS) const;

  /// @}
};

}

template<>
struct basic::BinaryCodingTraits<buildsystem::BuildValue::Kind> {
  typedef buildsystem::BuildValue::Kind Kind;
  
  static inline void encode(const Kind& value, BinaryEncoder& coder) {
    uint8_t tmp = uint8_t(value);
    assert(value == Kind(tmp));
    coder.write(tmp);
  }
  static inline void decode(Kind& value, BinaryDecoder& coder) {
    uint8_t tmp;
    coder.read(tmp);
    value = Kind(tmp);
  }
};

template<>
struct basic::BinaryCodingTraits<buildsystem::BuildValue> {
  static inline void encode(const buildsystem::BuildValue& value,
                            BinaryEncoder& coder) {
    value.encode(coder);
  }
  static inline void decode(buildsystem::BuildValue& value,
                            BinaryDecoder& coder) {
    value = buildsystem::BuildValue(coder);
  }
};

inline buildsystem::BuildValue::BuildValue(basic::BinaryDecoder& coder) {
  // Handle empty decode requests.
  if (coder.isEmpty()) {
    kind = BuildValue::Kind::Invalid;
    return;
  }
  
  coder.read(kind);
  switch (kind) {
  case Kind::Invalid:
  case Kind::VirtualInput:
  case Kind::MissingInput:
  case Kind::MissingOutput:
  case Kind::FailedInput:
  case Kind::FailedCommand:
  case Kind::PropagatedFailureCommand:
  case Kind::CancelledCommand:
  case Kind::SkippedCommand:
  case Kind::Target:
    break;
      
  case Kind::ExistingInput:
    coder.read(valueData.asExistingInput.fileInfo);
    break;
  case Kind::ExistingLink: {
    coder.read(valueData.asExistingLink.fileInfo);
    uint32_t targetSize;
    coder.read(targetSize);
    StringRef target;
    coder.readBytes(targetSize, target);
    valueData.asExistingLink.target = new char[targetSize + 1];
    memcpy(valueData.asExistingLink.target, target.data(), targetSize);
    valueData.asExistingLink.target[targetSize] = '\0';
    break;
  }
  case Kind::DirectoryContents: {
    coder.read(valueData.asDirectoryContents.fileInfo);
    coder.read(valueData.asDirectoryContents.contentsSize);
    StringRef contents;
    coder.readBytes(valueData.asDirectoryContents.contentsSize, contents);
    valueData.asDirectoryContents.contents =
      new char[valueData.asDirectoryContents.contentsSize];
    memcpy(valueData.asDirectoryContents.contents,
           contents.data(), contents.size());
    break;
  }
  case Kind::DirectoryTreeSignature:
    coder.read(valueData.asDirectoryTreeSignature.signature);
    break;
  case Kind::SuccessfulCommand:
    coder.read(valueData.asSuccessfulCommand.signature);
    coder.read(valueData.asSuccessfulCommand.numOutputs);
    valueData.asSuccessfulCommand.outputs =
      new BuildValue[valueData.asSuccessfulCommand.numOutputs];
    for (uint32_t i = 0; i != valueData.asSuccessfulCommand.numOutputs; ++i) {
      coder.read(valueData.asSuccessfulCommand.outputs[i]);
    }
    break;
  }
}

inline core::ValueType buildsystem::BuildValue::toData() const {
  basic::BinaryEncoder coder;
  coder.write(*this);
  return coder.contents();
}

inline void buildsystem::BuildValue::encode(basic::BinaryEncoder& coder) const {
  coder.write(kind);
  switch (kind) {
  case Kind::Invalid:
  case Kind::VirtualInput:
  case Kind::MissingInput:
  case Kind::MissingOutput:
  case Kind::FailedInput:
  case Kind::FailedCommand:
  case Kind::PropagatedFailureCommand:
  case Kind::CancelledCommand:
  case Kind::SkippedCommand:
  case Kind::Target:
    break;
      
  case Kind::ExistingInput:
    coder.write(valueData.asExistingInput.fileInfo);
    break;
  case Kind::ExistingLink: {
    coder.write(valueData.asExistingLink.fileInfo);
    uint32_t targetSize = (uint32_t) ::strlen(valueData.asExistingLink.target);
    coder.write(targetSize);
    coder.writeBytes(StringRef(valueData.asExistingLink.target, targetSize));
    break;
  }
  case Kind::DirectoryContents:
    coder.write(valueData.asDirectoryContents.fileInfo);
    coder.write(valueData.asDirectoryContents.contentsSize);
    coder.writeBytes(StringRef(valueData.asDirectoryContents.contents,
                               valueData.asDirectoryContents.contentsSize));
    break;
  case Kind::DirectoryTreeSignature:
    coder.write(valueData.asDirectoryTreeSignature.signature);
    break;
  case Kind::SuccessfulCommand:
    coder.write(valueData.asSuccessfulCommand.signature);
    coder.write(valueData.asSuccessfulCommand.numOutputs);
    for (uint32_t i = 0; i != valueData.asSuccessfulCommand.numOutputs; ++i) {
      coder.write(valueData.asSuccessfulCommand.outputs[i]);
    }
    break;
  }
}

}

#endif
