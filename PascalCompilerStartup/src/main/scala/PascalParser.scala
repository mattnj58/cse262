/* 
    Parser and AST builder for Pascal Compiler project
    CSE 262 - Programming Languages
    Fall 2017
    Name:
    Email:
*/

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, Positional, Reader, NoPosition}

case class Program(name:IDENTIFIER, io:List[IDENTIFIER], blk:Block) extends PascalASTNode
case class ConstDef(name:IDENTIFIER, value:Constant) extends PascalASTNode
case class VarDecl(name:List[IDENTIFIER], typ:PascalType) extends PascalASTNode
case class Block(cdlist:List[ConstDef],vdlist:List[VarDecl],stp:List[Statement]) extends PascalASTNode
case class Constant(sign:String,value:NUMBER) extends PascalASTNode 

class PascalType extends PascalASTNode

class SimpleType extends PascalType

case class NamedType(name:IDENTIFIER) extends PascalType

case class Relop(op:PascalASTNode, se1:PascalASTNode, se2:PascalASTNode) extends PascalASTNode
case class Binop(op:PascalASTNode, t1:PascalASTNode, t2:PascalASTNode) extends PascalASTNode
case class Unop(op:PascalASTNode, e:PascalASTNode) extends PascalASTNode

class Statement extends PascalASTNode
case class AssignmentStatement(lhs:PascalASTNode, rhs:PascalASTNode) extends Statement
case class CompoundStatement(list:List[Statement]) extends Statement
case class WhileStatement(cond:PascalASTNode, body:Statement) extends Statement
case class RepeatStatement(cond:PascalASTNode, body:Statement) extends Statement
case class IfStatement(cond:PascalASTNode, thenstat:Statement, elsestat:Statement) extends Statement
case class ForStatement(indexvar:IDENTIFIER, startexp:PascalASTNode, endexp:PascalASTNode, dir:PascalToken, statement:Statement) extends Statement 
case class OutputValue(e1:PascalASTNode, e2:Option[PascalASTNode], e3:Option[PascalASTNode]) extends PascalASTNode
case class WriteStatement(elist:List[OutputValue], eol:Boolean) extends Statement

class PascalTokenReader(tokens: Seq[PascalToken]) extends Reader[PascalToken] {
  override def first: PascalToken = { /* if (true) println(s"first: ${tokens.head}"); */ tokens.head }
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[PascalToken] = new PascalTokenReader(tokens.tail)
}

case class PascalParserError(msg: String) extends PascalCompilationError

object PascalParser extends Parsers {
    override type Elem = PascalToken
    val debug = true

    def identifier: Parser[IDENTIFIER] = {
        accept("identifier", { case id @ IDENTIFIER(name) => id })
    }

    def number: Parser[NUMBER] = {
        accept("number", { case n @ NUMBER(num) => n })
    }

    def sign: Parser[PascalToken] = PLUS | MINUS

    def constant: Parser[Constant] = opt( sign ) ~ number  ^^ {
        case Some(s) ~ n => Constant(s.toString, n) // HI 
        case None ~ n => Constant("", n)
    }

    def program: Parser[Program] = 

    def block: Parser[Block] = 

    def statement_part: Parser[List[Statement]] = BEGIN ~> statement_sequence <~ END

    def statement_sequence: Parser[List[Statement]] = repsep ( statement, SEMICOLON )   

    def statement: Parser[Statement] = structured_statement | simple_statement
    
    def simple_statement: Parser[Statement] = assignment_statement

    def assignment_statement: Parser[Statement] = 

    def structured_statement: Parser[Statement] = conditional_statement | compound_statement | repetitive_statement | write_statement

    def compound_statement: Parser[Statement] = 

    def repetitive_statement: Parser[Statement] = while_statement | repeat_statement | for_statement

    def while_statement: Parser[Statement] = 

    def repeat_statement: Parser[Statement] = 

    def conditional_statement: Parser[Statement] = if_statement

    def for_statement: Parser[Statement] = 

    def if_statement: Parser[Statement] = 

    def output_value: Parser[OutputValue] = 

    def write_statement: Parser[Statement] = 

    def expression: Parser[PascalASTNode] = 

    def relational_operator: Parser[PascalASTNode] = EQUALS | NOTEQUALTO | LESSTHAN | LESSTHANOREQUALTO | GREATERTHAN | 
                                            GREATERTHANOREQUALTO | IN

    def simple_expression: Parser[PascalASTNode] = 

    def addition_operator: Parser[PascalASTNode] = 

    def multiplication_operator: Parser[PascalASTNode] = 

    def term: Parser[PascalASTNode] = 

    def factor: Parser[PascalASTNode] = 
    

    def variable: Parser[PascalASTNode] = 

    def constant_definition_part: Parser[List[ConstDef]] = 

    def constant_definition: Parser[ConstDef] = 

    def variable_declaration_part: Parser[List[VarDecl]] = 

    def variable_declaration: Parser[VarDecl] = 

    def identifier_list: Parser[List[IDENTIFIER]] = repsep(identifier, COMMA )

    def atype: Parser[PascalType] = 

    def apply(tokens: Seq[PascalToken]) = {
        val reader = new PascalTokenReader(tokens)
        program(reader) match {
            case NoSuccess(msg, next) => throw new PascalParserError(msg)
            case Success(result, next) => result
        }
    }

}