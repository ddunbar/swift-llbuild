//===-- BuildValue.cpp ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llbuild/BuildSystem/BuildValue.h"

#include "llbuild/Basic/LLVM.h"

#include "llvm/Support/raw_ostream.h"

using namespace llbuild;
using namespace llbuild::buildsystem;

StringRef BuildValue::stringForKind(BuildValue::Kind kind) {
  switch (kind) {
#define CASE(kind) case Kind::kind: return #kind
    CASE(Invalid);
    CASE(VirtualInput);
    CASE(ExistingInput);
    CASE(ExistingLink);
    CASE(MissingInput);
    CASE(DirectoryContents);
    CASE(DirectoryTreeSignature);
    CASE(MissingOutput);
    CASE(FailedInput);
    CASE(SuccessfulCommand);
    CASE(FailedCommand);
    CASE(PropagatedFailureCommand);
    CASE(CancelledCommand);
    CASE(SkippedCommand);
    CASE(Target);
#undef CASE
  }
  return "<unknown>";
}
  
void BuildValue::dump(raw_ostream& os) const {
  os << "BuildValue(" << stringForKind(kind);
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
    
  case Kind::ExistingInput: {
    os << ", fileInfo=";
    getExistingInputFileInfo().dump(os);
    break;
  }    
  case Kind::ExistingLink: {
    os << ", fileInfo=";
    getExistingLinkFileInfo().dump(os);
    os << ", target=\"";
    os.write_escaped(getExistingLinkTarget());
    os << "\"";
    break;
  }
  case Kind::DirectoryContents: {
    os << ", fileInfo=";
    getDirectoryContentsFileInfo().dump(os);
    std::vector<StringRef> values = getDirectoryContents();
    os << ", contents=[";
    for (unsigned i = 0; i != values.size(); ++i) {
      if (i != 0) os << ", ";
      os << '"';
      os.write_escaped(values[i]);
      os << '"';
    }
    os << "]";
    break;
  }
  case Kind::DirectoryTreeSignature: {
    os << ", signature=" << getDirectoryTreeSignature();
    break;
  }
  case Kind::SuccessfulCommand: {
    os << ", signature=" << getSuccessfulCommandSignature();
    os << ", outputs=[";
    for (unsigned i = 0; i != getSuccessfulCommandNumOutputs(); ++i) {
      auto& info = getSuccessfulCommandOutput(i);
      if (i != 0) os << ", ";
      info.dump(os);

    }
    os << "]";
  }
  }
  os << ")";
}
