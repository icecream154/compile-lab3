#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <iostream>

using namespace llvm;
using namespace llvm::sys;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number_int = -5,
    tok_number_double = -6,
};

enum Types {
    type_int = 1,
    type_double = 2
};


static std::map<std::string, int> TypeValues;  //map typeString to int
static FILE *fip;
static std::string IdentifierStr; // Filled in if tok_identifier
static int NumValI;             // Filled in if tok_number_int
static double NumValD;             // Filled in if tok_number_double
static int ValType;               // Filled in if tok_number_double

static void InitializeTypeValue() {
    TypeValues["int"] = 1;
    TypeValues["double"] = 2;
}

/// gettok - Return the next token from standard input.
static int gettok() {

    static int LastChar = ' ';
    std::map<std::string, int>::iterator iter;

    // Skip any whitespace.
    while (isspace(LastChar))
        LastChar = fgetc(fip);

    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalpha(LastChar = fgetc(fip)) || isdigit(LastChar)) {
            IdentifierStr += LastChar;
        }
        if (IdentifierStr == "int") {
            ValType = type_int;
            return tok_def;
        } else if (IdentifierStr == "double") {
            ValType = type_double;
            return tok_def;
        } else if (IdentifierStr == "extern") {
            return tok_extern;
        } else {
            return tok_identifier;
        }
    }
    if (isdigit(LastChar)) {
        std::string str = std::to_string(LastChar - 48);
        LastChar = fgetc(fip);
        while (isdigit(LastChar)) {
            str.append(std::to_string(LastChar - 48));
            LastChar = fgetc(fip);
        }

        if (LastChar == 46) { // ascii : . -> 46
            str.append(".");
            LastChar = fgetc(fip);
            while (isdigit(LastChar)) {
                str.append(std::to_string(LastChar - 48));
                LastChar = fgetc(fip);
            }
            if (isspace(LastChar) || LastChar == EOF) {
                NumValD = atof(str.c_str());
                return tok_number_double;
            }
        } else if (isspace(LastChar) || LastChar == EOF) {
            NumValI = atoi(str.c_str());
            return tok_number_int;
        }
    }

    // Check for end of file.
    if (LastChar == EOF) return tok_eof;

    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    LastChar = fgetc(fip);
    return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//
// you don't have to modify this part. (of course it is ok to add something if needed)
namespace {

/// ExprAST - Base class for all expression nodes.
    class ExprAST {
    public:
        virtual ~ExprAST() = default;

        virtual Value *codegen() = 0;
    };

/// NumberExprAST - Expression class for numeric literals like "1.0".
    class NumberDoubleExprAST : public ExprAST {
        double Val;

    public:
        NumberDoubleExprAST(double Val) : Val(Val) {}

        Value *codegen() override;
    };

    class NumberIntExprAST : public ExprAST {
        int Val;

    public:
        NumberIntExprAST(int Val) : Val(Val) {}

        Value *codegen() override;
    };

/// VariableExprAST - Expression class for referencing a variable, like "a".
    class VariableExprAST : public ExprAST {
        std::string Name;

    public:
        VariableExprAST(const std::string &Name) : Name(Name) {}

        Value *codegen() override;
    };

/// BinaryExprAST - Expression class for a binary operator.
    class BinaryExprAST : public ExprAST {
        char Op;
        std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                      std::unique_ptr<ExprAST> RHS)
                : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

        Value *codegen() override;
    };

/// CallExprAST - Expression class for function calls.
    class CallExprAST : public ExprAST {
        std::string Callee;
        std::vector<std::unique_ptr<ExprAST>> Args;

    public:
        CallExprAST(const std::string &Callee,
                    std::vector<std::unique_ptr<ExprAST>> Args)
                : Callee(Callee), Args(std::move(Args)) {}

        Value *codegen() override;
    };

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
    class PrototypeAST {
        std::string Name;
        std::vector<std::string> Args;
        std::vector<int> ArgTypes;
        int FnType;

    public:
        PrototypeAST(const std::string &Name, std::vector<std::string> Args, std::vector<int> ArgTypes, int FnType)
                : Name(Name), Args(std::move(Args)), ArgTypes(std::move(ArgTypes)), FnType(FnType) {}

        Function *codegen();

        const std::string &getName() const { return Name; }

        const int getReturnType() { return FnType; }

        const std::vector<int> &getArgTypes() { return ArgTypes; }
    };

/// FunctionAST - This class represents a function definition itself.
    class FunctionAST {
        std::unique_ptr<PrototypeAST> Proto;
        std::unique_ptr<ExprAST> Body;

    public:
        FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                    std::unique_ptr<ExprAST> Body)
                : Proto(std::move(Proto)), Body(std::move(Body)) {}

        Function *codegen();
    };

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;

static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

///**********************************************

///use Parse*() function implemented in lab2

///**********************************************

/// LogError* - These are little helper functions for error handling.
/// you can add additional function to help you log error.
std::unique_ptr<ExprAST> LogError(const char *Str) {
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
    LogError(Str);
    return nullptr;
}

std::unique_ptr<FunctionAST> LogErrorF(const char *Str) {
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
    if (!isascii(CurTok))
        return -1;

    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0)
        return -1;
    return TokPrec;
}


/// numberexpr ::= number
/// example
static std::unique_ptr<ExprAST> ParseNumberExpr(int NumberType) {
    if (NumberType == type_double) {
        auto Result = std::make_unique<NumberDoubleExprAST>(NumValD);
        getNextToken(); // consume the number
        return std::move(Result);
    } else {
        auto Result = std::make_unique<NumberIntExprAST>(NumValI);
        getNextToken(); // consume the number
        return std::move(Result);
    }
}


/// identifier expr
/// <ident> or <callee>
//< ident > ::= [A Z_a z][0 9A Z_a z]∗
//< callee > ::= < ident > (ϵ| < exp > [, < exp >]∗)
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    //< ident >
    if (CurTok != tok_identifier)
        return LogError("Expected ident in ParseIdentifierExpr(");
    std::string IdName = IdentifierStr;
    getNextToken(); // eat FnName
    if (CurTok != '(') { //indent
        return std::move(std::make_unique<VariableExprAST>(IdName));
    }
    getNextToken(); // eat (.
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (CurTok != ')') {
        while (true) {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;
            if (CurTok == ')')
                break;

            if (CurTok != ',')
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }
    // Eat the ')'.
    getNextToken();
    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}


/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken(); // eat (.
    auto V = ParseExpression();
    if (!V)
        return nullptr;

    if (CurTok != ')')
        return LogError("expected ')'");
    getNextToken(); // eat ).
    return V;
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
        case tok_identifier: //< ident > | < callee >
            return ParseIdentifierExpr();
        case tok_number_int:
            return ParseNumberExpr(type_int);
        case tok_number_double:
            return ParseNumberExpr(type_double);
        case '(':
            return ParseParenExpr();

        default:
            return LogError("unknown token when expecting an expression");
    }
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
    // If this is a binop, find its precedence.
    while (true) {
        int TokPrec = GetTokPrecedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec)
            return LHS;

        // Okay, we know this is a binop.
        int BinOp = CurTok;
        getNextToken(); // eat binop

        // Parse the primary expression after the binary operator.
        auto RHS = ParsePrimary();
        if (!RHS)
            return nullptr;

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }

        // Merge LHS/RHS.
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}


/// expression
/// <exp> ::= (< exp >)| < const > | < ident > |< exp >< binop >< exp > | < callee >
static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS)
        return nullptr;
    return ParseBinOpRHS(0, std::move(LHS));
}

/// statement
/// <stmt>::= < exp >
/// example
static std::unique_ptr<ExprAST> ParseStatement() {
    auto E = ParseExpression();
    return E;
}

static std::unique_ptr<ExprAST> ParseBody() {
    if (CurTok != '{')
        return LogError("Expected '{' in body");
    getNextToken(); //eat '{'
    auto E = ParseStatement();
    if (!E)
        return nullptr;
    if (CurTok != '}')
        return LogError("Expected '}' in body");
    getNextToken(); //eat '}'
    return E;
}

/// prototype
/// <prototype>::= < type >< ident > (< paramlist >)
/// It can parse <prototype> without args.
static std::unique_ptr<PrototypeAST> ParsePrototype() {
    //< type >
    int FnType = ValType; //"int" or "double"
    getNextToken(); // eat ValType
    //< ident >
    if (CurTok != tok_identifier)
        return LogErrorP("Expected function name in prototype");

    std::string FnName = IdentifierStr;
    getNextToken(); // eat FnName

    if (CurTok != '(')
        return LogErrorP("Expected '(' in prototype");

    std::vector<std::string> ArgNames;
    std::vector<int> ArgTypes;
    do {
        getNextToken(); // eat '(' or '，'.
        if (CurTok != tok_def) {
            return LogErrorP("Expected type definition in ArgList");
        }
        ArgTypes.push_back(std::move(ValType));
        getNextToken(); // eat 'valtype'.
        if (CurTok != tok_identifier) {
            return LogErrorP("Expected identifier in ArgList");
        }
        ArgNames.push_back(std::move(IdentifierStr));
        getNextToken(); // eat 'valtype'.
    } while (CurTok == ',');

    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype");

    // success.
    getNextToken(); // eat ')'.

    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), std::move(ArgTypes), FnType);
}

/// definition ::= 'def' prototype expression
/// <function>
static std::unique_ptr<FunctionAST> ParseDefinition() {
    auto Proto = ParsePrototype();
    if (!Proto)
        return nullptr;
    if (auto E = ParseBody()) //body
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    return nullptr;
}

/// external ::= 'extern' prototype
/// <gdecl>
/// example
static std::unique_ptr<PrototypeAST> ParseExtern() {
    int isdef = getNextToken(); // eat extern.s
    if (isdef != tok_def)
        return LogErrorP("Expected type declaration");
    auto Proto = ParsePrototype();
    if (CurTok != ';')
        return LogErrorP("Expected ';' in global declaration");
    getNextToken(); // eat ';'
    return Proto;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value *> NamedValues;  // used to store the variables in the Function.
// in this lab it only store the args.
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;


Value *LogErrorV(const char *Str) {
    LogError(Str);
    return nullptr;
}

// getFunction(Name) can return a Function structure variable, F, to caller, which can 
// be used to creat a callee statement in codegen.
Function *getFunction(std::string Name) {
    // First, see if the function has already been added to the current module.
    if (auto *F = TheModule->getFunction(Name))
        return F;

    // If not, check whether we can codegen the declaration from some existing
    // prototype.
    auto FI = FunctionProtos.find(Name);
    if (FI != FunctionProtos.end())
        return FI->second->codegen();

    // If no existing prototype exists, return null.
    return nullptr;
}


/*TODO: Finish the codegen() function to implement the Code Generation part.
  We provide some implemented codegen function for reference, like 
  NumberDoubleExprAST::codegen(), Value *VariableExprAST::codegen(), and you
  can use these functions directly. 
*/


//example of codegen()
Value *NumberDoubleExprAST::codegen() {
    return ConstantFP::get(*TheContext, APFloat(Val));
}


//example of codegen()
Value *NumberIntExprAST::codegen() {
    return ConstantInt::get(*TheContext, APInt(32, Val));
}


//example of codegen()
Value *VariableExprAST::codegen() {
    // Look this variable up in the function.
    Value *V = NamedValues[Name];
    if (!V)
        return LogErrorV("Unknown variable name");
    return V;
}


//To Do
Value *BinaryExprAST::codegen() {
    Value *leftVal = LHS->codegen();
    Value *rightVal = RHS->codegen();
    bool leftIsDouble = leftVal->getType()->isDoubleTy();
    bool rightIsDouble = rightVal->getType()->isDoubleTy();

    // contains double type
    if (leftIsDouble || rightIsDouble) {
        // change int to double type
        if (!leftIsDouble) {
            leftVal = Builder->CreateUIToFP(leftVal, Type::getDoubleTy(*TheContext), "tmp");
        }
        if (!rightIsDouble) {
            rightVal = Builder->CreateUIToFP(rightVal, Type::getDoubleTy(*TheContext), "tmp");
        }
        switch (Op) {
            case '+':
                return Builder->CreateFAdd(leftVal, rightVal, "addtmp");
            case '-':
                return Builder->CreateFSub(leftVal, rightVal, "subtmp");
            case '*':
                return Builder->CreateFMul(leftVal, rightVal, "multmp");
            default :
                return Builder->CreateFCmpULT(leftVal, rightVal, "lttmp");
        }
    } else {
        switch (Op) {
            case '+':
                return Builder->CreateAdd(leftVal, rightVal, "addtmp");
            case '-':
                return Builder->CreateSub(leftVal, rightVal, "subtmp");
            case '*':
                return Builder->CreateMul(leftVal, rightVal, "multmp");
            default :
                return Builder->CreateICmpULT(leftVal, rightVal, "lttmp");
        }
    }
}

//To Do
Value *CallExprAST::codegen() {
    Function *function = getFunction(Callee);
    std::vector<Value *> args;
    for (auto &arg: Args)
        args.push_back(arg->codegen());
    return Builder->CreateCall(function, ArrayRef<Value *>(args));
}

// an imcomplete codegen function. It can generate IR for prototype whose types of args
// and return value are all double.
Function *PrototypeAST::codegen() {
    // Make the function type:  double(double,double) etc.
    std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));

    std::vector<Type *> typeVector;
    for (auto argType: ArgTypes) {
        if (argType == type_int)
            typeVector.push_back(Type::getInt32Ty(*TheContext));
        if (argType == type_double)
            typeVector.push_back(Type::getDoubleTy(*TheContext));
    }
    Type *retType = FnType == type_double ? Type::getDoubleTy(*TheContext) :
                    Type::getInt32Ty(*TheContext);
    FunctionType *FT =
            FunctionType::get(retType, ArrayRef<Type *>(typeVector), false);

    Function *F =
            Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // Set names for all arguments.
    unsigned Idx = 0;
    for (auto &Arg: F->args())
        Arg.setName(Args[Idx++]);

    return F;
}

// an imcomplete codegen function.
// You should finish the ToDo part in this function
Function *FunctionAST::codegen() {
    // Transfer ownership of the prototype to the FunctionProtos map, but keep a
    // reference to it for use below.
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    Function *TheFunction = getFunction(P.getName());
    if (!TheFunction)
        return nullptr;

    // Create a new basic block to start insertion into.
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    NamedValues.clear();
    for (auto &Arg: TheFunction->args())
        NamedValues[std::string(Arg.getName())] = &Arg;

    if (Value *RetVal = Body->codegen()) {
        //****************
        //ToDo: correctly create the RetVal use Builder.
        //****************

        // return type is double
        if (TheFunction->getReturnType()->isDoubleTy() && !RetVal->getType()->isDoubleTy())
            RetVal = Builder->CreateUIToFP(RetVal, Type::getDoubleTy(*TheContext), "tmp");
        // return type is int
        if (!TheFunction->getReturnType()->isDoubleTy() && RetVal->getType()->isDoubleTy())
            RetVal = Builder->CreateFPToUI(RetVal, Type::getInt32PtrTy(*TheContext), "tmp");
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency.
        verifyFunction(*TheFunction);
        return TheFunction;
    }

    // Error reading body, remove function.
    TheFunction->eraseFromParent();
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level
//===----------------------------------------------------------------------===//
//don't modify this part

static void InitializeModuleAndPassManager() {
    // Open a new context and module.
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit", *TheContext);

    // Create a new builder for the module.
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

static void HandleDefinition() {
    if (auto FnAST = ParseDefinition()) {
        if (auto *FnIR = FnAST->codegen()) {
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern() {
    if (auto ProtoAST = ParseExtern()) {
        if (auto *FnIR = ProtoAST->codegen()) {
            fprintf(stderr, "Read extern: ");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}


/// top ::= definition | external | expression | ';'
static void MainLoop() {
    while (true) {
        switch (CurTok) {
            case tok_eof:
                return;
            case tok_def:
                HandleDefinition();
                break;
            case tok_extern:
                HandleExtern();
                break;
            default:
                std::cout << "invalid input" << std::endl;
                getNextToken();
                break;
        }
    }
}
//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//
//don't modify this part

int main(int argc, char *argv[]) {
    if (argc < 2) {
        errs() << "You need to specify the file to compile";
        return 1;
    }
    char *FileName = argv[1];
    fip = fopen(FileName, "r");
    if (fip == nullptr) {
        errs() << "The file '" << FileName << "' is not existed";
        return 1;
    }

    InitializeTypeValue();

    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.
    getNextToken();

    InitializeModuleAndPassManager();
    MainLoop();

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    auto TargetTriple = sys::getDefaultTargetTriple();
    TheModule->setTargetTriple(TargetTriple);

    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!Target) {
        errs() << Error;
        return 1;
    }

    auto CPU = "generic";
    auto Features = "";

    TargetOptions opt;
    auto RM = Optional<Reloc::Model>();
    auto TheTargetMachine =
            Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);
    TheModule->setDataLayout(TheTargetMachine->createDataLayout());

    auto Filename = "output.o";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

    if (EC) {
        errs() << "Could not open file: " << EC.message();
        return 1;
    }

    legacy::PassManager pass;
    auto FileType = LLVMTargetMachine::CGFT_ObjectFile;

    if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        errs() << "TheTargetMachine can't emit a file of this type";
        return 1;
    }

    pass.run(*TheModule);
    dest.flush();

    outs() << "Wrote " << Filename << "\n";

    return 0;
}