grammar Grammar;

@header{
import grammar.*;
}

start returns [Grammar grammar]
        : NTERM { $grammar = new Grammar($NTERM.text); }
          (imports[$grammar])?
          (rule_[$grammar] ';')+
          EOF
        ;

imports[Grammar grammar]
        : '#import'
          L_SQ
          SCRIPT { $grammar.getImports().add($SCRIPT.text.substring(1, $SCRIPT.text.length() - 1)); }
          (COMMA SCRIPT { $grammar.getImports().add($SCRIPT.text.substring(1, $SCRIPT.text.length() - 1)); })*
          R_SQ
        ;


rule_[Grammar grammar]
        : TERM EQ STRING    { $grammar.addTR(new TRImpl($TERM.text, $STRING.text)); }
        | TERM ':' STRING     { $grammar.addTR(new RRImpl($TERM.text, $STRING.text)); }
        | GENERIC '=' STRING     { $grammar.addGeneric(new Generic($GENERIC.text, $STRING.text)); }
        | ntr     { $grammar.addNTR($ntr.v); }
        ;

ntr returns [NTR v]
        : NTERM args nt_ret EQ    { $v = new NTR($NTERM.text, $args.v, $nt_ret.v); }
          rightPart                                 { $v.addRule($rightPart.v); }
          ('|' rightPart                             { $v.addRule($rightPart.v); })*
        ;

args returns [List<Arg> v]
        : L_SQ               { $v = new ArrayList<>(); }
          arg                   { $v.add($arg.v); }
          (COMMA arg            { $v.add($arg.v); })*
          R_SQ
        |                       { $v = new ArrayList<>(); }
        ;

nt_ret returns [List<Arg> v]
        : 'returns' L_SQ        { $v = new ArrayList<>(); }
          arg                   { $v.add($arg.v); }
          (COMMA arg            { $v.add($arg.v); })*
          R_SQ
        |                       { $v = new ArrayList<>(); }
        ;

arg returns [Arg v]
        : l = var ':' r = var    { $v = new Arg($l.v, $r.v); }
        ;

var returns [String v]
        : TERM          { $v = $TERM.text; }
        | NTERM         { $v = $NTERM.text; }
        | GENERIC       { $v = $GENERIC.text; }
        ;

rightPart returns [List<RuleToken> v]
        :               { $v = new ArrayList<>(); }
        (ruleToken      { $v.add($ruleToken.v); })+
        ;

ruleToken returns [RuleToken v]
        : TERM          { $v = new T($TERM.text); }
        | NTERM         { NT t = new NT($NTERM.text); }
          ('(' param    { t.addArg($param.v); }
          (COMMA param  { t.addArg($param.v); })*
          ')')?         { $v = t; }
        | SCRIPT          { $v = new Script($SCRIPT.text); }
        ;

param returns [String v]
        : SCRIPT  { $v = $SCRIPT.text.substring(1, $SCRIPT.text.length() - 1); }
        | var   { $v = $var.v; }
        ;


COMMA    : ',';
EQ   : '=';
L_SQ  : '[';
R_SQ : ']';

GENERIC: '#'TERM;
TERM   : [A-Z][a-zA-Z0-9_]*;
NTERM  : [a-z][a-zA-Z0-9_]*;

STRING : '"' (~('"'))* '"';
SCRIPT   : '{' (~[{}]+ SCRIPT?)* '}';

WS : [ \t\r\n] -> skip;