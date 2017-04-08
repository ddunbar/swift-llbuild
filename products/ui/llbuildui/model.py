import struct

from sqlalchemy import *
from sqlalchemy.orm import relation, relationship
from sqlalchemy.ext.declarative import declarative_base

# DB Declaration

Base = declarative_base()

class KeyName(Base):
    __tablename__ = "key_names"

    id = Column(Integer, nullable=False, primary_key=True)
    name = Column('key', String, nullable=False)

    def __repr__(self):
        return "%s%r" % (
            self.__class__.__name__, (self.id, self.name))

class RuleResult(Base):
    __tablename__ = "rule_results"

    id = Column(Integer, nullable=False, primary_key=True)
    key_id = Column(Integer, ForeignKey(KeyName.id),
                     nullable=False)
    value_bytes = Column("value", Integer, nullable=False)
    built_at = Column(Integer, nullable=False)
    computed_at = Column(Integer, nullable=False)

    key = relation(KeyName)
    dependencies = relationship('RuleDependency')

    def __repr__(self):
        return "%s%r" % (
            self.__class__.__name__, (self.id, self.key, self.value,
                                      self.built_at, self.computed_at))

    @property
    def value(self):
        return BuildValue(self.value_bytes)

class RuleDependency(Base):
    __tablename__ = "rule_dependencies"

    rule_id = Column(Integer, ForeignKey(RuleResult.id),
                     nullable=False, primary_key=True)
    key_id = Column(Integer, ForeignKey(KeyName.id),
                     nullable=False, primary_key=True)

    rule = relation(RuleResult)
    key = relation(KeyName)
    
    def __repr__(self):
        return "%s%r" % (
            self.__class__.__name__, (self.rule_id, self.key))

###

class BuildValue(object):
    # FIXME: This is a manually Python translation of the C++
    # llbuild::buildsystem::BuildValue type, which is unfortunate, but it isn't
    # available via an API we can access directly yet.
    
    kinds = [
        "Invalid",
        "VirtualInput", "ExistingInput", "ExistingLink", "MissingInput",
        "DirectoryContents", "DirectoryTreeSignature",
        "MissingOutput", "FailedInput",
        "SuccessfulCommand", "FailedCommand",
        "PropagatedFailureCommand", "CancelledCommand", "SkippedCommand",
        "Target",
    ]
    
    def __init__(self, data):
        bytes = str(data)
        
        # The first byte is the kind.
        if bytes:
            self.kind = self.__class__.kinds[struct.unpack("<B", bytes[0])[0]]
            bytes = bytes[1:]
        else:
            self.kind = "Invalid"

        # The next item is the signature, if used.
        if self.hasCommandSignature:
            self.signature = struct.unpack("<Q", bytes[:8])[0]
            bytes = bytes[8:]
        else:
            self.signature = None
            
        # The outputs follow, if used.
        if self.hasOutputInfo:
            numOutputs = struct.unpack("<I", bytes[:4])[0]
            bytes = bytes[4:]
            self.outputs = []
            for i in range(numOutputs):
                # Read the file information.
                self.outputs.append(FileInfo(bytes[:48]))
                bytes = bytes[48:]
        else:
            self.outputs = None

        # The strings follow, if used.
        if self.hasStringList:
            stringsLength = struct.unpack("<Q", bytes[:8])[0]
            bytes = bytes[8:]
            if stringsLength == 0:
                self.strings = []
            else:
                stringData = bytes[:stringsLength]
                bytes = bytes[stringsLength:]
                assert len(stringData) == stringsLength
                assert stringData[-1] == '\0'
                self.strings = stringData[:-1].split("\0")
        else:
            self.strings = None

        assert len(bytes) == 0

    @property
    def hasCommandSignature(self):
        return self.kind in ("SuccessfulCommand", "DirectoryTreeSignature")

    @property
    def hasStringList(self):
        return self.kind in ("DirectoryContents",)

    @property
    def hasOutputInfo(self):
        return self.kind in ("ExistingInput", "SuccessfulCommand",
                             "DirectoryContents")

    def __repr__(self):
        output = "BuildValue(kind=%r" % self.kind
        if self.signature is not None:
            output += ", signature=%0x" % self.signature
        if self.outputs is not None:
            output += ", outputs=%r" % self.outputs
        if self.strings is not None:
            output += ", strings=%r" % self.strings
        output += ")"
        return output

class FileInfo(object):
    def __init__(self, bytes):
        (self.device, self.inode, self.mode, self.size,
         modTimeSec, modTimeNano) = struct.unpack("<QQQQQQ", bytes)
        self.modTime =  (modTimeSec, modTimeNano)

    def __repr__(self):
        return "FileInfo(device=%r, inode=%#0x, mode=%r, size=%r, mtime=(%r, %r))" % (
            self.device, self.inode, self.mode, self.size,
            self.modTime[0], self.modTime[1])
