#define MAXTOK   512
#define MAXOUT   512
#define MAXSTACK 512

enum {
  TK_NUM=1,
  TK_OP,
  TK_LPAREN,
  TK_RPAREN,
  TK_END,
  TK_BAD
};
class Tok {
  I64 kind;     // TK_*
  F64 num;      // for numbers
  I64 op;       
  I64 pos;      
};

I64 IsSpace(I64 c) { return c==' '||c=='\t'||c=='\n'||c=='\r'; }
I64 IsDigit(I64 c) { return c>='0'&&c<='9'; }
I64 IsOp(I64 c)    { return c=='+'||c=='-'||c=='*'||c=='/'; }

I64 OpPrec(I64 op)
{
  if (op=='+'||op=='-') return 1;
  if (op=='*'||op=='/') return 2;
  return 0;
}

I64 OpLeftAssoc(I64 op)
{
  return TRUE;
}

U0 TokPrint(Tok *t)
{
  if (t->kind==TK_NUM)
    "NUM(%f)@%d", t->num, t->pos;
  else if (t->kind==TK_OP)
    "OP(%c)@%d", t->op, t->pos;
  else if (t->kind==TK_LPAREN)
    "LPAREN@%d", t->pos;
  else if (t->kind==TK_RPAREN)
    "RPAREN@%d", t->pos;
  else if (t->kind==TK_END)
    "END";
  else
    "BAD@%d", t->pos;
}

I64 Lex(Str *s, Tok toks[MAXTOK])
{
  I64 i=0, n=0;
  I64 c;
  F64 val;
  I64 sawDot;

  while (TRUE) {
    while (IsSpace(s[i])) i++;

    c = s[i];
    if (!c) {
      toks[n].kind = TK_END;
      toks[n].pos  = i;
      n++;
      return n;
    }

    if (c=='(') {
      toks[n].kind = TK_LPAREN;
      toks[n].pos  = i;
      n++; i++;
      continue;
    }
    if (c==')') {
      toks[n].kind = TK_RPAREN;
      toks[n].pos  = i;
      n++; i++;
      continue;
    }

    // Number: allow leading dot like ".5"
    if (IsDigit(c) || c=='.') {
      val = 0;
      sawDot = FALSE;

      // parse using manual scan (because why not)
      // integer part
      if (c!='.') {
        while (IsDigit(s[i])) {
          val = val*10 + (s[i]-'0');
          i++;
        }
      }

      // fractional part
      if (s[i]=='.') {
        F64 place = 0.1;
        sawDot = TRUE;
        i++;
        while (IsDigit(s[i])) {
          val = val + (s[i]-'0')*place;
          place = place*0.1;
          i++;
        }
      }

      // if it was just "." -> bad token
      if (!sawDot && !IsDigit(c)) {
        toks[n].kind = TK_BAD;
        toks[n].pos  = i;
        n++;
        continue;
      }

      toks[n].kind = TK_NUM;
      toks[n].num  = val;
      toks[n].pos  = i;
      n++;
      if (n>=MAXTOK-2) {
        toks[n].kind = TK_END;
        toks[n].pos  = i;
        n++;
        return n;
      }
      continue;
    }

    // operator
    if (IsOp(c)) {
      toks[n].kind = TK_OP;
      toks[n].op   = c;
      toks[n].pos  = i;
      n++; i++;
      continue;
    }

    // unknown char
    toks[n].kind = TK_BAD;
    toks[n].pos  = i;
    n++; i++;
    if (n>=MAXTOK-2) {
      toks[n].kind = TK_END;
      toks[n].pos  = i;
      n++;
      return n;
    }
  }
}

I64 ToRPN(Tok in[MAXTOK], Tok out[MAXOUT], Tok ops[MAXSTACK], I64 *errPos)
{
  I64 i=0, o=0, sp=0;

  while (TRUE) {
    Tok *t = &in[i];

    if (t->kind==TK_END) break;

    if (t->kind==TK_BAD) {
      *errPos = t->pos;
      return FALSE;
    }

    if (t->kind==TK_NUM) {
      out[o++] = *t;
    } else if (t->kind==TK_OP) {
      I64 p1 = OpPrec(t->op);

      while (sp>0) {
        Tok *top = &ops[sp-1];
        if (top->kind!=TK_OP) break;

        I64 p2 = OpPrec(top->op);
        if (p2>p1 || (p2==p1 && OpLeftAssoc(t->op))) {
          out[o++] = *top;
          sp--;
        } else break;
      }
      ops[sp++] = *t;
    } else if (t->kind==TK_LPAREN) {
      ops[sp++] = *t;
    } else if (t->kind==TK_RPAREN) {
      I64 found = FALSE;
      while (sp>0) {
        Tok top = ops[sp-1];
        sp--;
        if (top.kind==TK_LPAREN) { found=TRUE; break; }
        out[o++] = top;
      }
      if (!found) { *errPos=t->pos; return FALSE; }
    }

    if (o>=MAXOUT-2 || sp>=MAXSTACK-2) { *errPos=t->pos; return FALSE; }
    i++;
  }

  while (sp>0) {
    Tok top = ops[sp-1];
    sp--;
    if (top.kind==TK_LPAREN) { *errPos=top.pos; return FALSE; }
    out[o++] = top;
    if (o>=MAXOUT-2) { *errPos=top.pos; return FALSE; }
  }

  out[o].kind = TK_END;
  out[o].pos  = in[i].pos;
  o++;
  return TRUE;
}

I64 EvalRPN(Tok rpn[MAXOUT], F64 *res, I64 trace)
{
  F64 st[MAXSTACK];
  I64 sp=0, i=0;

  if (trace) "\n--- VM TRACE ---\n";

  while (TRUE) {
    Tok *t = &rpn[i];
    if (t->kind==TK_END) break;

    if (t->kind==TK_NUM) {
      if (sp>=MAXSTACK-1) { if (trace) "stack overflow\n"; return FALSE; }
      st[sp++] = t->num;
      if (trace) "push %f (sp=%d)\n", t->num, sp;
    } else if (t->kind==TK_OP) {
      if (sp<2) { if (trace) "stack underflow\n"; return FALSE; }
      F64 b = st[--sp];
      F64 a = st[--sp];
      F64 c;

      if (t->op=='+') c = a + b;
      else if (t->op=='-') c = a - b;
      else if (t->op=='*') c = a * b;
      else if (t->op=='/') c = a / b;
      else return FALSE;

      st[sp++] = c;
      if (trace) "op  %c : %f %c %f -> %f (sp=%d)\n", t->op, a, t->op, b, c, sp;
    } else {
      return FALSE;
    }

    i++;
    if (i>=MAXOUT-1) return FALSE;
  }

  if (sp!=1) { if (trace) "sp ended at %d (expected 1)\n", sp; return FALSE; }
  *res = st[0];
  return TRUE;
}

// A second evaluator: recursive descent (just to argue with the RPN version during stress tests)
class RD {
  Str *s;
  I64 i;
  I64 bad;
};

U0 RDSkip(RD *r){ while (IsSpace(r->s[r->i])) r->i++; }

F64 RDParseExpr(RD *r);

F64 RDParseNumber(RD *r)
{
  RDSkip(r);
  I64 start=r->i;
  F64 val=0;
  I64 has=FALSE;

  if (r->s[r->i]=='.') {
    // ".5"
    r->i++;
    F64 place=0.1;
    if (!IsDigit(r->s[r->i])) { r->bad=TRUE; return 0; }
    while (IsDigit(r->s[r->i])) {
      val += (r->s[r->i]-'0')*place;
      place*=0.1;
      r->i++;
    }
    return val;
  }

  while (IsDigit(r->s[r->i])) {
    val = val*10 + (r->s[r->i]-'0');
    r->i++;
    has=TRUE;
  }

  if (r->s[r->i]=='.') {
    r->i++;
    F64 place=0.1;
    while (IsDigit(r->s[r->i])) {
      val += (r->s[r->i]-'0')*place;
      place*=0.1;
      r->i++;
      has=TRUE;
    }
  }

  if (!has) { r->bad=TRUE; r->i=start; }
  return val;
}

F64 RDParseFactor(RD *r)
{
  RDSkip(r);
  if (r->s[r->i]=='(') {
    r->i++;
    F64 x = RDParseExpr(r);
    RDSkip(r);
    if (r->s[r->i]!=')') { r->bad=TRUE; return 0; }
    r->i++;
    return x;
  }

  // unary minus for extra spice
  if (r->s[r->i]=='-') {
    r->i++;
    return -RDParseFactor(r);
  }

  return RDParseNumber(r);
}

F64 RDParseTerm(RD *r)
{
  F64 x = RDParseFactor(r);
  while (!r->bad) {
    RDSkip(r);
    I64 c = r->s[r->i];
    if (c!='*' && c!='/') break;
    r->i++;
    F64 y = RDParseFactor(r);
    if (c=='*') x = x*y;
    else x = x/y;
  }
  return x;
}

F64 RDParseExpr(RD *r)
{
  F64 x = RDParseTerm(r);
  while (!r->bad) {
    RDSkip(r);
    I64 c = r->s[r->i];
    if (c!='+' && c!='-') break;
    r->i++;
    F64 y = RDParseTerm(r);
    if (c=='+') x = x+y;
    else x = x-y;
  }
  return x;
}

I64 EvalRD(Str *s, F64 *res)
{
  RD r;
  r.s=s; r.i=0; r.bad=FALSE;
  *res = RDParseExpr(&r);
  RDSkip(&r);
  if (r.bad) return FALSE;
  if (r.s[r.i]!=0) return FALSE;
  return TRUE;
}

// Random expression generator for stress testing.
// Keeps it simple: balanced parentheses-ish, limited depth.
I64 RndRange(I64 lo, I64 hi) { return lo + (RandU32 % (hi-lo+1)); }

U0 AppendChar(Str *buf, I64 *len, I64 c)
{
  buf[*len] = c;
  (*len)++;
  buf[*len] = 0;
}

U0 AppendNum(Str *buf, I64 *len)
{
  I64 x = RndRange(0, 99);
  // occasionally decimal
  if ((RandU32 % 5)==0) {
    I64 y = RndRange(0, 99);
    *len += SPrint(buf+*len, "%d.%d", x, y);
  } else {
    *len += SPrint(buf+*len, "%d", x);
  }
}

U0 GenExpr(Str *buf, I64 *len, I64 depth)
{
  if (depth<=0 || (RandU32%3)==0) {
    AppendNum(buf,len);
    return;
  }

  if ((RandU32%4)==0) {
    AppendChar(buf,len,'(');
    GenExpr(buf,len,depth-1);
    AppendChar(buf,len,')');
    return;
  }

  GenExpr(buf,len,depth-1);
  AppendChar(buf,len,' ');
  I64 ops[4] = {'+','-','*','/'};
  AppendChar(buf,len, ops[RandU32%4]);
  AppendChar(buf,len,' ');
  GenExpr(buf,len,depth-1);
}

U0 StressTest(I64 rounds)
{
  Tok toks[MAXTOK], rpn[MAXOUT], opstk[MAXSTACK];
  I64 k, errPos;
  F64 a,b;

  "\n--- STRESS TEST (%d rounds) ---\n", rounds;

  for (k=0;k<rounds;k++) {
    Str expr[256];
    I64 len=0;
    expr[0]=0;

    GenExpr(expr,&len, RndRange(2,5));

    I64 tn = Lex(expr,toks);
    if (!ToRPN(toks,rpn,opstk,&errPos)) {
      "skip bad: %s\n", expr;
      continue;
    }
    if (!EvalRPN(rpn,&a,FALSE)) {
      "skip vm:  %s\n", expr;
      continue;
    }
    if (!EvalRD(expr,&b)) {
      "skip rd:  %s\n", expr;
      continue;
    }

    // allow tiny floating error
    if (Abs(a-b) > 1e-9) {
      "\nMISMATCH!\n";
      "expr: %s\n", expr;
      "rpn : %f\n", a;
      "rd  : %f\n", b;
      return;
    }

    if ((k%25)==0) ".";
  }

  "\nOK. no fights today.\n";
}

U0 DumpTokens(Tok toks[MAXTOK])
{
  I64 i=0;
  "\nTokens:\n";
  while (TRUE) {
    Tok *t=&toks[i];
    TokPrint(t); "\n";
    if (t->kind==TK_END) break;
    i++;
    if (i>=MAXTOK-1) break;
  }
}

U0 DumpRPN(Tok rpn[MAXOUT])
{
  I64 i=0;
  "\nRPN:\n";
  while (TRUE) {
    Tok *t=&rpn[i];
    if (t->kind==TK_END) { "END\n"; break; }
    TokPrint(t); "\n";
    i++;
    if (i>=MAXOUT-1) break;
  }
}

U0 main()
{
  Tok toks[MAXTOK], rpn[MAXOUT], opstk[MAXSTACK];
  I64 errPos;
  F64 result;

  "WeirdCalc (HolyC) — type an expression\n";
  "Commands: :q quit, :t toggle trace, :s stress test\n";

  I64 trace = FALSE;

  while (TRUE) {
    "\n> ";
    Str *line = Gets;

    if (!line) continue;

    if (!StrCmp(line, ":q")) break;

    if (!StrCmp(line, ":t")) {
      trace = !trace;
      "trace = %d\n", trace;
      continue;
    }

    if (!StrCmp(line, ":s")) {
      StressTest(300);
      continue;
    }

    I64 tn = Lex(line, toks);
    if (tn<=0) { "lex failed\n"; continue; }

    if (!ToRPN(toks, rpn, opstk, &errPos)) {
      "parse error near pos %d\n", errPos;
      continue;
    }

    DumpTokens(toks);
    DumpRPN(rpn);

    if (!EvalRPN(rpn, &result, trace)) {
      "eval error\n";
      continue;
    }

    "\n= %f\n", result;
  }

  "\nbye.\n";
}
