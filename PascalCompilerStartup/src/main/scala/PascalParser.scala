/* 
    Parser and AST builder for Pascal Compiler project
    CSE 262 - Programming Languages
    Fall 2017
    Name: Matthew Wong
    Email: myw219
*/

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

// IF~ LPAREN ~ expression ~ opt (ELSE ~statement) ~repsep(expression, colon) =>
// case _~ exp ~ opt
//none
//some(ELSE~statement)

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

    def program: Parser[Program] = PROGRAMx ~ identifier ~ LPAREN ~ identifier_list ~ RPAREN ~ SEMICOLON ~ block ~ PERIOD ^^{
        case _~identifierx~_~identifier_listx~_~_~blockx~_ => {
            Program(identifierx, identifier_listx, blockx)
        }
    }

    def block: Parser[Block] = constant_definition_part ~ variable_declaration_part ~ statement_part ^^{
        case constant_definition_partx~variable_declaration_partx~statement_partx => {
            Block(constant_definition_partx, variable_declaration_partx,statement_partx)
        }
    }

    def statement_part: Parser[List[Statement]] = BEGIN ~> statement_sequence <~ END 

    def statement_sequence: Parser[List[Statement]] = repsep(statement, SEMICOLON )

    def statement: Parser[Statement] = structured_statement | simple_statement
    
    def simple_statement: Parser[Statement] = assignment_statement

    def assignment_statement: Parser[Statement] = variable ~ ASSIGN ~ expression ^^ {
        case v~_~e => {
            AssignmentStatement(v, e)
        }
    }

    def structured_statement: Parser[Statement] = conditional_statement | compound_statement | repetitive_statement | write_statement

    def compound_statement: Parser[Statement] = BEGIN ~> statement_sequence <~ END ^^ {
        case statement_seq => {
            CompoundStatement(statement_seq)
        } 
    }

    def repetitive_statement: Parser[Statement] = while_statement | repeat_statement | for_statement

    def while_statement: Parser[Statement] = WHILE ~ expression ~ DO ~ statement ^^{
        case _~expression~_~statement => WhileStatement(expression,statement)
    }

    def repeat_statement: Parser[Statement] = REPEAT ~ statement_sequence ~ UNTIL ~ expression ^^ {
        case _~stateseq~_~expres => RepeatStatement(expres,CompoundStatement(stateseq)) //stateseq is the incorrect type -> statement type
    }

    def conditional_statement: Parser[Statement] = if_statement

    def for_statement: Parser[Statement] = FOR ~ identifier ~ ASSIGN ~ expression ~ (TO|DOWNTO) ~ expression ~ DO ~ statement ^^{
        case _~identx~_~assExpresx~ TO ~toExpressx~_~doStatx=> ForStatement(identx,assExpresx, toExpressx, TO , doStatx)
        case _~identx~_~assExpresx ~ DOWNTO ~ toExpressx ~_~ doStatx => ForStatement(identx, assExpresx, toExpressx, DOWNTO, doStatx)
    }

    //None type is not allowed and Some(se1) needs to be instantiated
    def if_statement: Parser[Statement] = IF ~ expression ~ THEN  ~ statement ~ opt(ELSE ~> statement) ^^{
        case _~expressionx~_~statementx~Some(elseStat) => {
            IfStatement(expressionx, statementx, elseStat)
        }
        case _~expressoinx ~_~ statementx ~ none => {
            IfStatement(expressoinx, statementx, CompoundStatement(List[Statement]()))
        }
    }

    //e1~e2 not allowed 
    def output_value: Parser[OutputValue] = expression ~ opt(COLON ~> expression ~ opt( COLON ~> expression)) ^^{
        case e1 ~ Some(e2~Some(e3)) => OutputValue(e1, Some(e2), Some(e3))
        case e1 ~ Some(e2~None) => OutputValue(e1, Some(e2), None)
        case e1 ~ None => OutputValue(e1, None, None)
    }

    def write_statement: Parser[Statement] = (WRITELN | WRITE) ~ LPAREN ~ repsep(output_value, COMMA) ~RPAREN ^^{
        case eol ~_~elist~_=> {
            WriteStatement(elist, eol == WRITELN)
        }
    }

    //unop function finds any type needs PascalASTNode same with AssignmentStatement
    def expression: Parser[PascalASTNode] = simple_expression ~ opt(relational_operator ~ simple_expression) ^^{
        case simp_express1 ~ Some(relate_op ~ simp_express2)=> OutputValue(simp_express1, Some(relate_op), Some(simp_express2))
        //case exps ~ Some (rop ~ exps2) => Relop(rop, exps, exps2)
        case exps ~ Some(relate_up ~ none) => OutputValue(exps, Some(relate_up), None)
        case exps ~ None=> OutputValue(exps, None, None)
    }
    
    def relational_operator: Parser[PascalASTNode] = EQUALS | NOTEQUALTO | LESSTHAN | LESSTHANOREQUALTO | GREATERTHAN | 
                                            GREATERTHANOREQUALTO | IN

    def simple_expression: Parser[PascalASTNode] = opt(sign) ~ term ~ rep(addition_operator ~ term) ^^{
        case None ~ t ~ ttail =>{
            ttail.foldLeft(t) { case(t1, ao ~ t2) => Binop(ao, t1, t2) }
        } 
        case Some(sign) ~ t ~ ttail => {
            val x = ttail.foldLeft(t) {case (t1,ao ~t2) => Binop(ao, t1, t2) }
            Unop(sign,x)
        }
    }

    def addition_operator: Parser[PascalASTNode] = PLUS | MINUS | OR

    def multiplication_operator: Parser[PascalASTNode] = TIMES|DIVIDE|DIV|MOD|AND

    def term: Parser[PascalASTNode] = factor ~ rep(multiplication_operator ~ factor) ^^ {
        case tFact ~ ttail => ttail.foldLeft(tFact) {case(tFact, mo~fact) => Binop(tFact, mo, fact) }
    }

    def factor: Parser[PascalASTNode] = (LPAREN ~> expression <~ RPAREN) |number|variable ^^{
        case fExpress => fExpress
        case num => num
        case vari => vari
    }

    def variable: Parser[PascalASTNode] = identifier

    def constant_definition_part: Parser[List[ConstDef]] = rep1sep(constant_definition,identifier) <~ SEMICOLON

    def constant_definition: Parser[ConstDef] = identifier ~ COLON ~ constant ^^ {
        case ident~_~const=> ConstDef(ident,const)
    }

    def variable_declaration_part: Parser[List[VarDecl]] = rep1sep(variable_declaration,identifier) <~ SEMICOLON

    def variable_declaration: Parser[VarDecl] = identifier_list ~ COLON ~ atype ^^{
        case id_list ~ _ ~ vtype => {
            VarDecl(id_list, vtype) //vtype is an incorrect variable type needs to be PascalASTNode
        } 
    }

    def identifier_list: Parser[List[IDENTIFIER]] = repsep(identifier, COMMA) 

    def atype: Parser[PascalType] = identifier ^^ {
        case id => NamedType(id)
    }

    def apply(tokens: Seq[PascalToken]) = {
        val reader = new PascalTokenReader(tokens)
        program(reader) match {
            case NoSuccess(msg, next) => throw new PascalParserError(msg)
            case Success(result, next) => result
        }
    }
}