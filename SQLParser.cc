#include <v8.h>
#include <node.h>

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_no_skip.hpp>
#include <string>

namespace ensighten
{
    namespace qi = boost::spirit::qi;
    namespace ascii = boost::spirit::ascii;

    ///////////////////////////////////////////////////////////////////////////////
    //  Our sql parser grammer
    ///////////////////////////////////////////////////////////////////////////////
    template <typename Iterator>
    struct sqlparser : qi::grammar<Iterator, ascii::space_type>
    {
        sqlparser() : sqlparser::base_type(start)
        {  
		  using qi::char_;
		  using qi::int_;
		  using qi::float_;
		  using qi::double_;
		  using qi::lit;
		  using qi::lexeme;
		  using qi::no_skip;
		  using ascii::no_case;

		  keywords = 
              no_case["AND"]
            | no_case["AS"]
            | no_case["ASC"]
            | no_case["BY"]
            | no_case["CASE"]
            | no_case["CONCAT"]
            | no_case["COUNT"]
            | no_case["CROSS"]
            | no_case["DATE_ADD"]
            | no_case["DATE_FORMAT"]
            //| no_case["DESC"]
            | no_case["DISTINCT"]
            | no_case["ELSE"]
            | no_case["FOR UPDATE"]
            | no_case["FROM"]
            | no_case["GROUP"]
            | no_case["IN"]
            | no_case["IS"]
            | no_case["INNER"]
            | no_case["INET_NTOA"]
            | no_case["INET_ATON"]
	        | no_case["INSERT"]
            | no_case["INTO"]
            | no_case["LEFT"]
            | no_case["LIMIT"]
            | no_case["NATURAL"]
            | no_case["NOT"]
            | no_case["NULL"]
            | no_case["OFFSET"]
            | no_case["ORDER"]
            | no_case["OR"]
            | no_case["ON"]
            | no_case["OUTER"]
            | no_case["RIGHT"]
            | no_case["SAMPLE_SIZE"]
            | no_case["SELECT"]
            | no_case["SET"]
            | no_case["SUM"]
            | no_case["THEN"]
            | no_case["UPDATE"]   
            | no_case["VALUES"]
            | no_case["WHERE"]
            | no_case["WHEN"]
            ;

		  name =   lexeme[char_("a-zA-Z_") >> *(char_("a-zA-Z_0-9"))] 
				 - keywords ;

		  columnName    = name;
		  columnAlias   = name;
		  tableName     = name;
		  tableAlias    = name;
		  newTableAlias = name;
		  schemaName    = name;

		  rowCountInt = int_;

		  string =    lit("'")
				   >> *(lit("\\\'") | lit("\\\\") | lit("\\\""))
				   >> -( +(char_ - '"' - '\\' - "'") % ( lit("\\\"") | lit("\\\'") | lit("\\\\")))
				   >> *(lit("\\\'") | lit("\\\\") | lit("\\\"")) 
				   >> lit("'")
				   ;

		  space  = lit(' ') | lit('\t') | lit('\n');

		  value =   string
				  | (int_ >> ',' >> int_)
				  | int_
				  | double_
				  | no_case["NULL"]
				  ;

		  // A value. Parameters can be indexed, for example 
		  // ?1 meaning the first parameter. 
		  term =   ( Case                                 )
				 | ( CaseWhen                             )
				 | ( value                                )
				 | ( tableAlias >> lit('.') >> columnName )
				 | ( columnName                           )
				 | ( lit('?') >> int_                     )
				 | ( Function                             )
				 | ( (lit('-') | lit('+')) >> term        )
				 | ( lit('(') >> expression >> lit(')')   )
				 | ( select                               )
				 ;


		  // Functions:
          // count, concat, dateFormat
          Function =   count
                     | concat
                     | dateFormat
                     | dateAdd
                     | distinct
                     | sum
                     | inet_ntoa
                     | inet_aton
                     ;

          count =    no_case["COUNT"] >> '('
                  >> ( lit('*') | ( -(no_case["DISTINCT"]) >> expression ) )
                  >> ')'
                  ;

          concat =   no_case["CONCAT"] >> '('
                  >> expression >> ',' >> ( expression % ',' )
                  >> ')'
                  ;
  
          distinct =    no_case["DISTINCT"]
                     >> expression
                     ; 

          sum =   no_case["SUM"]
               >> '(' >> columnName >> ')'
               ;

          inet_ntoa =    no_case["INET_NTOA"]
                      >> '(' >> expression >> ')'
                      ;

          inet_aton =    no_case["INET_ATON"]
                      >> '(' >> expression >> ')'
                      ;

          dateUnit =   no_case["MINUTE"]
                     | no_case["SECOND"]
                     | no_case["HOUR"]
                     | no_case["DAY"]
                     ;

          dateAdd =  no_case["DATE_ADD"] >> '('
                  >> expression >> ','
                  >> lexeme[ no_case["INTERVAL"] >> +space ] >> expression
                  >> dateUnit
                  >> ')'
                  ;

          dateFormat =    no_case["DATE_FORMAT"] >> '('
                      >>  expression >> ',' >> string 
                      >> ')'
                      ;

          Case =  +(
                       no_case["CASE"] >> expression
                    >> no_case["WHEN"] >> expression
                    >> no_case["THEN"] >> expression
                   )
               >> -(
                       no_case["ELSE"] >> expression
                   )
               >>      no_case["END"]
               ;

          CaseWhen =    +(
                               no_case["CASE"] 
                            >> no_case["WHEN"] >> expression
                            >> no_case["THEN"] >> expression
                         )
                     >> -(
                               no_case["ELSE"] >> expression
                         )
                     >>        no_case["END"]
                    ;

          expression   = andCondition % ( lexeme[ no_case["OR"]  >> +space ] );

          andCondition = condition    % ( lexeme[ no_case["AND"] >> +space ] ) ;

          condition =   ( operand >> - conditionRightHandSide )
                      | ( no_case["NOT"] >> condition )
                      | ( no_case["EXISTS"] >> '(' >> select >> ')' )
                      ;

          operand = summand % lit("||") ;

          summand = factor  % ( lit('+') | lit('-') ) ;

          factor  = term    % ( lit('*') | lit('/') ) ;

          compare = lit("<>") | lit("<=") | lit(">=") | lit("!=")
                              | lit("=" ) | lit(">" ) | lit("<" )
                    ;                   

          conditionRightHandSide =
                  ( compare >> (   operand
                                 | ( ( no_case["ALL"] | no_case["ANY"] | no_case["SOME"] )
                                      >> '(' >> select >> ')'
                                   )
                               ) 
                  )                       
               |  ( no_case["IS"] >> -(no_case["NOT"]) >> no_case["NULL"] )
               |  ( no_case["IS"] >> -(no_case["NOT"]) 
                                  >> -(no_case["DISTINCT"] >> no_case["FROM"])
                                  >> operand 
                  )
               |  ( no_case["BETWEEN"] >> operand >> no_case["AND"] >> operand )
               |  ( no_case["IN"] >> '(' >> (   (select)
                                              | (expression % ',') ) >> ')' 
                  )
               |  ( - no_case["NOT"] >> no_case["LIKE"] >> operand 
                              >> -( no_case["ESCAPE"] >> string ) 
                  )
               |  ( - no_case["NOT"] >> no_case["REGEXP"] >> operand )
               ;

     
  order =    (int_ | expression)
          >> -(no_case["ASC"] | no_case["DESC"])
          >> -(no_case["NULLS"] >> (no_case["FIRST"] | no_case["LAST"]))
          ;

    
  selectExpression =   '*'
                     | ( tableAlias >> lit('.') >> lit('*') )
                     | ( expression >> -( -no_case["AS"] >> columnAlias ) )
                     ;

  tableExpression =  (  
                          ( -( schemaName >> '.' ) >> tableName ) 
                       |  ( '(' >> select >> ')' )  
                     )
                     >> -( -no_case["AS"] >> newTableAlias )
                     >> *(
                             (
                                ((no_case["LEFT"] | no_case["RIGHT"]) >> -no_case["OUTER"])
                              | -no_case["INNER"]
                              | no_case["CROSS"]
                              | no_case["NATURAL"]
                             )
                          >> no_case["JOIN"]
                          >> tableExpression
                          >> -(no_case["ON"] >> expression)
                         )
                    ;
                
/*
 *  SELECT statement
 */
  select =    no_case["SELECT"]
           >> -( no_case["TOP"] >> term )
           >> -( no_case["DISTINCT"] | no_case["ALL"] )
           >>  ( selectExpression % ',' )
           >> no_case["FROM"]
           >>  ( tableExpression % ',' )
           >> -( no_case["WHERE"] >> expression )
           >> -( (no_case["GROUP BY"] >> expression) % ',' )
           >> -( no_case["HAVING"] >> expression )
           >> -( 
                 (   
                     ( no_case["UNION"] >> -(no_case["ALL"]) )
                   | ( no_case["MINUS"] )
                   | ( no_case["EXCEPT"] )
                   | ( no_case["INTERSECT"] )
                 ) 
                 >> select
               )
           >> -( (no_case["ORDER BY"] >> order) % ',' )
           >> -( 
                    (no_case["LIMIT"] >> expression)
                >> -(no_case["OFFSET"] >> expression)
                >> -(no_case["SAMPLE_SIZE"] >> rowCountInt)         
               )
           >> -no_case["FOR UPDATE"]
           ;


/*
 *  INSERT statement
 */ 

  insert =    no_case["INSERT INTO"]
           >> tableName
           >> -('(' >> (columnName % ',') >> ')')
           >> (
                 (
                      no_case["VALUES"] 
                   >> (( 
                           '('
                        >> (no_case["DEFAULT"] | (expression % ','))
                        >> ')'
                      ) % ',')
                 )
               | (   select
                  >> -(   no_case["ON DUPLICATE KEY UPDATE"]
                       >> ((   -(tableAlias>>'.')
                            >> columnName 
                            >> '=' 
                            >> expression) % ',') 
                          )         
                      )
                 )
            ;


/*
 *  UPDATE statement
 */
  update =    no_case["UPDATE"]
           >> tableName
           >> -(-no_case["AS"] >> newTableAlias)
           >> no_case["SET"]
           >> (
                (    columnName
                  >> '='
                  >> (no_case["DEFAULT"] | expression)
                ) % ','
              )
           >> -(no_case["WHERE"] >> expression)
           ;

/*
 *  DELETE statement
 */
  Delete =    no_case["DELETE FROM"]
           >> tableName
           >> -(no_case["WHERE"] >> expression)
           ;


/*
 *  TRUNCATE statement
 */
  truncate =    no_case["TRUNCATE"]
             >> -no_case["TABLE"]
             >> tableAlias
             ;


/*
 *  SHOW TABLE STATUS statement
 */
  showTableStatus =   no_case["SHOW TABLE STATUS"]
                   >> (
                          ((no_case["FROM"] | no_case["IN"]) >> newTableAlias)
                        | (
                              (no_case["LIKE"] >> "'" >> tableAlias >> "'")
                            | (no_case["WHERE"] >> expression)
                          )
                      )
                   ;


/*
 *  REPLACE statement
 */
  replace =   no_case["REPLACE"]
           >> -(no_case["LOW_PRIORITY"] | no_case["DELAYED"])
           >> no_case["INTO"]
           >> tableAlias
           >> -('(' >> (columnName % ',') >> ')')
           >> (no_case["VALUES"] | no_case["VALUE"])
           >> lit('(')
           >> ( (expression % ',') | no_case["DEFAULT"] )
           >> lit(')')
           ;



  start =  select
         | insert
         | update
         | Delete
         | truncate
         | showTableStatus
         | replace
         ;
        }

            qi::rule<Iterator, ascii::space_type> keywords;
    qi::rule<Iterator, ascii::space_type> select;
    qi::rule<Iterator, ascii::space_type> term;
    qi::rule<Iterator, ascii::space_type> selectExpression;
    qi::rule<Iterator, ascii::space_type> tableExpression;
    qi::rule<Iterator, ascii::space_type> expression;
    qi::rule<Iterator, ascii::space_type> order;
    qi::rule<Iterator, ascii::space_type> value;
    qi::rule<Iterator, ascii::space_type> columnName;
    qi::rule<Iterator, ascii::space_type> Function;
    qi::rule<Iterator, ascii::space_type> Case;
    qi::rule<Iterator, ascii::space_type> CaseWhen;
    qi::rule<Iterator, ascii::space_type> tableAlias;
    qi::rule<Iterator, ascii::space_type> string;
    qi::rule<Iterator, ascii::space_type> name;
    qi::rule<Iterator, ascii::space_type> count;
    qi::rule<Iterator, ascii::space_type> concat;
    qi::rule<Iterator, ascii::space_type> dateFormat;
    qi::rule<Iterator, ascii::space_type> dateAdd;
    qi::rule<Iterator, ascii::space_type> dateUnit;
    qi::rule<Iterator, ascii::space_type> andCondition;
    qi::rule<Iterator, ascii::space_type> condition;
    qi::rule<Iterator, ascii::space_type> operand;
    qi::rule<Iterator, ascii::space_type> conditionRightHandSide;
    qi::rule<Iterator, ascii::space_type> summand;
    qi::rule<Iterator, ascii::space_type> factor;
    qi::rule<Iterator, ascii::space_type> compare;
    qi::rule<Iterator, ascii::space_type> columnAlias;
    qi::rule<Iterator, ascii::space_type> schemaName;
    qi::rule<Iterator, ascii::space_type> tableName;
    qi::rule<Iterator, ascii::space_type> newTableAlias;
    qi::rule<Iterator, ascii::space_type> rowCountInt;
    qi::rule<Iterator, ascii::space_type> distinct;
    qi::rule<Iterator, ascii::space_type> sum;
    qi::rule<Iterator, ascii::space_type> inet_ntoa;
    qi::rule<Iterator, ascii::space_type> inet_aton;
    
    qi::rule<Iterator, ascii::space_type> insert;
    qi::rule<Iterator, ascii::space_type> update;
    qi::rule<Iterator, ascii::space_type> Delete;
    qi::rule<Iterator, ascii::space_type> truncate;
    qi::rule<Iterator, ascii::space_type> showTableStatus;
    qi::rule<Iterator, ascii::space_type> replace;

    qi::rule<Iterator> space;
    qi::rule<Iterator, ascii::space_type> start;
    };
}

using namespace node;
using namespace v8;

class SQLParser: ObjectWrap
{
public:

  static Persistent<FunctionTemplate> s_ct;
  static void Init(Handle<Object> target)
  {
    HandleScope scope;

    Local<FunctionTemplate> t = FunctionTemplate::New(New);

    s_ct = Persistent<FunctionTemplate>::New(t);
    s_ct->InstanceTemplate()->SetInternalFieldCount(1);
    s_ct->SetClassName(String::NewSymbol("SQLParser"));
	
    NODE_SET_PROTOTYPE_METHOD(s_ct, "parse", Parse);

    target->Set(String::NewSymbol("SQLParser"),
                s_ct->GetFunction());
  }

  SQLParser() 
  {
  }

  ~SQLParser()
  {
  }

  static Handle<Value> New(const Arguments& args)
  {
    HandleScope scope;
    SQLParser* hw = new SQLParser();
    hw->Wrap(args.This());
    return args.This();
  }

  static Handle<Value> Parse(const Arguments& args)
  {
	using boost::spirit::ascii::space;
    typedef std::string::const_iterator iterator_type;
    typedef ensighten::sqlparser<iterator_type> sqlparser;

	sqlparser calc;
	
	const v8::String::AsciiValue v8str(args[0]);
	std::string str(*v8str);

	std::string::const_iterator iter = str.begin();
    std::string::const_iterator end = str.end();
    bool r = phrase_parse(iter, end, calc, space);
	
	if (r && iter == end){
	  return Boolean::New(true);
	}
	else
	{
	  return Boolean::New(false);
	}
  }

};

Persistent<FunctionTemplate> SQLParser::s_ct;

extern "C" {
  static void init (Handle<Object> target)
  {
    SQLParser::Init(target);
  }

  NODE_MODULE(SQLParser, init);
}
