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

//We have computed First+(<exp>) :{'(',<const>,<ident>} by hand
#define isExpression(tok) ((tok) == '(' || (tok) == tok_number_int || (tok) == tok_number_double || (tok) == tok_identifier)

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.

#define IDENT_CHAR(A) (isalpha(A)||isdigit(A))

enum Token {
  tok_invalid = 0,
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

static void InitializeTypeValue(){
  TypeValues["int"] = 1;
  TypeValues["double"] = 2;
}

/// gettok - Return the next token from standard input.
static int gettok() {
///*****************************************

///use gettok implemented in lab1  

///*****************************************
  static int LastChar = ' ';
  std::map<std::string, int>::iterator iter;

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = fgetc(fip);

  
  if (isalpha(LastChar)) { 
  //------------------------------------------
  // TODO: identifier // definition // extern
  //------------------------------------------- 
     int state = 0;
     int accept_tok;
     while(true){
       switch(state){
         case 0:
		switch(LastChar){
		  case 'i': state = 1;break;
	          case 'd': state = 5;break;
	          case 'e': state = 11;break;
	          default: state = 4;break;
		}
		IdentifierStr = std::string(1,(char)LastChar);
		accept_tok = tok_identifier;
	        break;
         case 1:
		state = LastChar=='n'?2:4;
		accept_tok = tok_identifier;
	        break;
         case 2:
		if(LastChar == 't'){
		   state = 3;
		   accept_tok = tok_def;
		   ValType = type_int;
		}else {
		   state = 4;
		   accept_tok = tok_identifier;
		}
	        break;
         case 3:
         case 4:
         case 10:
         case 16:
		state = 4;
		accept_tok = tok_identifier;
	        break;
         case 5:
		state = LastChar=='o'?6:4;
		accept_tok = tok_identifier;
	        break;
         case 6:
		state = LastChar=='u'?7:4;
		accept_tok = tok_identifier;
	        break;
         case 7:
		state = LastChar=='b'?8:4;
		accept_tok = tok_identifier;
	        break;
         case 8:
		state = LastChar=='l'?9:4;
		accept_tok = tok_identifier;
	        break;
         case 9:
		if(LastChar == 'e'){
		  state = 10;
		  accept_tok = tok_def;
		  ValType = type_double;
		}else{
		  state = 4;
		  accept_tok = tok_identifier;
		}
	        break;
         case 11:
		state = LastChar=='x'?12:4;
		accept_tok = tok_identifier;
	        break;
         case 12:
		state = LastChar=='t'?13:4;
		accept_tok = tok_identifier;
	        break;
         case 13:
		state = LastChar=='e'?14:4;
		accept_tok = tok_identifier;
	        break;
         case 14:
		state = LastChar=='r'?15:4;
		accept_tok = tok_identifier;
	        break;
         case 15:
		if(LastChar == 'n'){
		  state = 16;
		  accept_tok = tok_extern;
		}else{
		  state = 4;
		  accept_tok = tok_identifier;
		}
	        break;
       }
       LastChar = fgetc(fip);
       if(!IDENT_CHAR(LastChar)){
           return accept_tok;
       }
       IdentifierStr += LastChar;
     }
  }

  
  if (isdigit(LastChar)){
  //-----------------------------------
  // TODO: number_int // number_double
  //-----------------------------------
     int state = 0;
     int accept_tok = tok_number_int;
     IdentifierStr = std::string(1,(char)LastChar);
     LastChar = fgetc(fip);
     while(true){
     	switch(state){
	   case 0:
	   	if(isdigit(LastChar)){
		  state = 0;
	    	  IdentifierStr += LastChar;
		  accept_tok = tok_number_int;
                  LastChar = fgetc(fip);
		}else if(LastChar == '.'){
		  state = 1;
		  accept_tok = tok_invalid;
                  LastChar = fgetc(fip);
		}else{
		  state = 2;
		}
	     break;
	   case 1:
	        if(isdigit(LastChar)){
		   state = 1;
		   if(accept_tok == tok_invalid){
	             IdentifierStr += ".";
		   }
		   IdentifierStr += LastChar;
		   accept_tok = tok_number_double;
                   LastChar = fgetc(fip);
		}else{
		   state = 2;
		}
	     break;
	  case 2:
	     if(accept_tok == tok_number_int){
	        NumValI = stoi(IdentifierStr);
	     }else if(accept_tok == tok_number_double){
		NumValD = stod(IdentifierStr);
	     }else{
		LastChar = '.';
		NumValI = stoi(IdentifierStr);
		return tok_number_int;
	     }
	     return accept_tok;
	     break;
	}
     }
  }

  // Check for end of file.
  if (LastChar == EOF)
    return tok_eof;

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
  const int getReturnType() {return FnType;}
  const std::vector<int> &getArgTypes() {return ArgTypes;}
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

static std::unique_ptr<ExprAST> ParseExpression(); 

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


/*TODO: Finish the Parse*() function to implement the Parser.
  We provide some implemented Parse* function for reference, like 
  ParseNumberExpr(), ParseExtern(), which are marked with "example", and you can use these functions directly. 
  >>>note: You can add some other Parse*() function to help you achieve this goal,
  >>>e.g. ParseParenExpr() which parenexpr ::= '(' expression ')'.
*/




/// numberexpr ::= number
/// example
static std::unique_ptr<ExprAST> ParseNumberExpr(int NumberType) {
  if (NumberType == type_double){
    auto Result = std::make_unique<NumberDoubleExprAST>(NumValD);
    getNextToken(); // consume the number
    return std::move(Result);
  }
  else{
    auto Result = std::make_unique<NumberIntExprAST>(NumValI);
    getNextToken(); // consume the number
    return std::move(Result);
  }
}


/// identifierexpr
/// <ident> or <callee>
/// TODO
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string ident = IdentifierStr;
  getNextToken();//eat ident
  if(CurTok == '('){
    std::vector<std::unique_ptr<ExprAST>> Args;
    getNextToken();
    if(!isExpression(CurTok) && CurTok != ')')
      return LogError("Expected <exp> or ')' in <callee>"); 
    while(true){
      if(isExpression(CurTok)){
        auto exp = ParseExpression();
        if(exp)
          Args.push_back(std::move(exp));
      }else if(CurTok == ','){
	getNextToken();
	if(!isExpression(CurTok))
          return LogError("Expected <exr> in <callee>");
      }else{
	break;
      }
    }
    if(CurTok != ')')
      return LogError("Expected ')' in <callee>");
    getNextToken();
    return std::make_unique<CallExprAST>(ident,std::move(Args));
  }else{
    return std::make_unique<VariableExprAST>(ident);
  }
}


/// expression
/// <exp>
/// TODO
static std::unique_ptr<ExprAST> ParseStar(){
   if(CurTok == '('){
     getNextToken();
     if(!isExpression(CurTok))
       return LogError("Expected expression in statement");
     auto exp = ParseExpression();
     if(CurTok != ')')
       return LogError("Expected ')' in statement");
     getNextToken();
     return exp;
   }else if(CurTok == tok_number_int){
      return ParseNumberExpr(type_int);
   }else if(CurTok == tok_number_double)
      return ParseNumberExpr(type_double);
   else if(CurTok == tok_identifier){
      return ParseIdentifierExpr();
   }
   else
      return LogError("Invalid expression in statement");
}
static std::unique_ptr<ExprAST> ParseFactorPrime(std::unique_ptr<ExprAST> expr){
   getNextToken();
   auto star = ParseStar();
   if(!star)
    return LogError("Expected second operate of '*'");
   auto exprMul = std::make_unique<BinaryExprAST>('*',std::move(expr),std::move(star));
   if(CurTok != '*')
     return exprMul;
   return ParseFactorPrime(std::move(exprMul));
}
static std::unique_ptr<ExprAST> ParseFactor(){
   auto star = ParseStar();
   if(!star || CurTok != '*')
     return star;
   return ParseFactorPrime(std::move(star));
}	
static std::unique_ptr<ExprAST> ParseTermPrime(std::unique_ptr<ExprAST> expr){
   char binop = (char)CurTok;
   getNextToken();
   auto factor = ParseFactor();
   if(!factor)
     return LogError("Expected second operate of '+/-'");
   auto exprAddOrMinus = std::make_unique<BinaryExprAST>(binop,std::move(expr),std::move(factor));
   if(CurTok != '+' && CurTok != '-')
     return exprAddOrMinus;
   auto termPrime = ParseTermPrime(std::move(exprAddOrMinus));
   return termPrime;
}
static std::unique_ptr<ExprAST> ParseTerm(){
   auto factor = ParseFactor();
   if(!factor)
     return factor;
   if(CurTok == '+' || CurTok == '-'){
     auto termPrime = ParseTermPrime(std::move(factor));
     return termPrime;
   }else
     return factor;
}	
static std::unique_ptr<ExprAST> ParseExprPrime(std::unique_ptr<ExprAST> expr){
   getNextToken();
   auto term = ParseTerm();
   if(!term)
    return LogError("Expected second operate of '<'");
   auto exprLess = std::make_unique<BinaryExprAST>('<',std::move(expr),std::move(term));
   if(CurTok != '<')
     return exprLess;
   auto expPrime = ParseExprPrime(std::move(exprLess));
   return expPrime;
}
static std::unique_ptr<ExprAST> ParseExpression() {
  auto term = ParseTerm();
  if(!term || CurTok != '<')
    return term;
  auto expPrime = ParseExprPrime(std::move(term));
  return expPrime;
}

/// statement 
/// <stmt>
/// example
static std::unique_ptr<ExprAST> ParseStatement() {
  auto E = ParseExpression();
  return E;
}

/// prototype
/// <prototype>
/// an imcomplete parse function. It can parse <prototype> without args.
/// TODO
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  int FnType = ValType;
  getNextToken(); // eat ValType
  if (CurTok != tok_identifier)
    return LogErrorP("Expected function name in prototype");
  
  std::string FnName = IdentifierStr;
  getNextToken(); // eat FnName

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");
  getNextToken(); // eat '('.
  std::vector<std::string> ArgNames;
  std::vector<int> ArgTypes;
  // Maybe a FA is better here.
  if(CurTok != tok_def && CurTok != ')')
    return LogErrorP("Expected arg type or ')' in prototype");
  while(true){
    if(CurTok == tok_def){
      int argType = ValType;
      getNextToken();
      if(CurTok != tok_identifier)
	 return LogErrorP("Expected arg name in prototype");
      std::string argName = IdentifierStr;
      ArgNames.push_back(argName);
      ArgTypes.push_back(argType);
      getNextToken();
      if(CurTok != ',' && CurTok != ')')
	return LogErrorP("Expected ',' or ')' in prototype");
    }else if(CurTok == ','){
      getNextToken();
      if(CurTok != tok_def)
	return LogErrorP("Expected arg in prototype");
    }else
      break;
  }
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  // success.
  getNextToken(); // eat ')'.

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), std::move(ArgTypes), FnType);
}

/// definition ::= 'def' prototype expression
/// <function>
/// TODO
static std::unique_ptr<FunctionAST> ParseDefinition() {
  auto Proto = ParsePrototype();
  if(CurTok != '{')
   return LogErrorF("Expected '{' in Function");
  getNextToken();
  auto Stmt = ParseStatement();
  if(CurTok != '}')
   return LogErrorF("Expected '}' in Function");
  getNextToken();
  return std::make_unique<FunctionAST>(std::move(Proto),std::move(Stmt));
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
  return ConstantInt::get(*TheContext, APInt(32,Val));
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
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  bool isDouble = L->getType()->isDoubleTy() || R->getType()->isDoubleTy();
  if(isDouble){
    if(!L->getType()->isDoubleTy())
      L = Builder ->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "tmp");
    if(!R->getType()->isDoubleTy())
      R = Builder ->CreateUIToFP(R, Type::getDoubleTy(*TheContext), "tmp");
    switch(Op){
      case '+': return Builder->CreateFAdd(L,R,"addtmp");break;
      case '-': return Builder->CreateFSub(L,R,"subtmp");break;
      case '*': return Builder->CreateFMul(L,R,"multmp");break;
      default : return Builder->CreateFCmpULT(L,R,"lttmp");break;
    }
  }else{
    switch(Op){
      case '+': return Builder->CreateAdd(L,R,"addtmp");break;
      case '-': return Builder->CreateSub(L,R,"subtmp");break;
      case '*': return Builder->CreateMul(L,R,"multmp");break;
      default : return Builder->CreateICmpULT(L,R,"lttmp");break;
    }
  }
  return nullptr;
}

//To Do
Value *CallExprAST::codegen() {
  Function *func = getFunction(Callee);
  std::vector<Value *>putargs;
  for(auto &arg:Args)
    putargs.push_back(arg->codegen());
  ArrayRef<Value *> argsRef(putargs);
  return Builder->CreateCall(func,argsRef);
}

// an imcomplete codegen function. It can generate IR for prototype whose types of args
// and return value are all double.
Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  /*std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
  FunctionType *FT =
      FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);*/

  std::vector<Type *> types;
  for(auto argType: ArgTypes){
     switch(argType){
       case type_int:types.push_back(Type::getInt32Ty(*TheContext));break;
       case type_double:types.push_back(Type::getDoubleTy(*TheContext));break;
       default:break;
     }
  }
  ArrayRef<Type *> typeRef(types);
  Type *retType = FnType==type_int?Type::getInt32Ty(*TheContext):Type::getDoubleTy(*TheContext);
  FunctionType *FT = FunctionType::get(retType,typeRef,false);
  Function *F =
      Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args())
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
  for (auto &Arg : TheFunction->args())
    NamedValues[std::string(Arg.getName())] = &Arg;

  if (Value *RetVal = Body->codegen()) {
    //****************
    //ToDo: correctly create the RetVal use Builder.
    //****************
    if(RetVal->getType()->isDoubleTy() && !TheFunction->getReturnType()->isDoubleTy())
      RetVal =  Builder->CreateFPToUI(RetVal, Type::getInt32Ty(*TheContext), "tmp");
    else if(!RetVal->getType()->isDoubleTy() && TheFunction->getReturnType()->isDoubleTy())
      RetVal = Builder->CreateUIToFP(RetVal,Type::getDoubleTy(*TheContext),"tmp");
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

int main(int argc, char* argv[]) {  
  if(argc < 2){
    errs() << "You need to specify the file to compile";
    return 1;
  }
  char* FileName = argv[1];
  fip = fopen(FileName, "r");
  if(fip == nullptr){
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
