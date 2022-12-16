%{
    #include <stdio.h>
    #include <string.h>
    int yylex(void);
    int yyerror(char*);
    void add_to_sym_tab(char*);
    int idx(char*);    
    void init_lines();
    void ending_lines();
    void add_alloc();
    void backpatch(int*, int*, int, int);
    void add_label();
    int count = 0, accepted = 1, ir_count = 1, buf_count = 0, start_label = 0;
    char sym_tab[2][20][10], cur_type[10], buf[50][500];
    int ir_num[20];
    FILE* fp;
%}

%union {
    char* str;
    int ival;
    double dval;
    struct exp_struct {
        double val;
        char type[20];
        int ir_name;
        struct blist {
            int id[10], offset[10];
            int count;
        } true_list, false_list;
    } eval;
    struct stmt_struct {
        struct slist {
            int id[10], offset[10];
            int count;
        } next_list;
    } sval;
}

%token INT DOUBLE COMMA SEMICOLON BEG END IF 
%token <ival> INTEGER_CONSTANT 
%token <dval> DOUBLE_CONSTANT
%token <str> ID
%type <eval> EXP TERM FACTOR relExp
%type <sval> stmt assignStmt ifStmt stmtList
%type <ival> M

%%
prog :  { init_lines(); } 
        funcDef { printf(accepted ? "Accepted. IR generated in out.ll\n" : "Not Accepted\n"); } 
        { ending_lines(); } ;

funcDef : type ID 
        { sprintf(buf[buf_count++], "define %s @%s() #0 {\n", !strcmp(cur_type, "int") ? "i32" : "double", $2); }
        '(' argList ')' BEG declList stmtList M END { 
    backpatch($9.next_list.id, $9.next_list.offset, $9.next_list.count, $10);
    add_label();
    sprintf(buf[buf_count++], "\tret i32 0\n}\n\n"); 
}

argList : arg COMMA arg | arg | ;

arg : type ID { add_to_sym_tab($2); };

declList : declList decl | decl;

decl : type varList SEMICOLON;

varList : ID { add_to_sym_tab($1); add_alloc(); } COMMA varList 
        | ID { add_to_sym_tab($1); add_alloc(); }
        ;

type : INT { strcpy(cur_type, "int"); }
     | DOUBLE { strcpy(cur_type, "double"); }
     ;

stmtList : stmtList M stmt {
    backpatch($1.next_list.id, $1.next_list.offset, $1.next_list.count, $2);
    $$ = $3;
}
    | stmt { $$ = $1; }
    ;

M : { $$ = ir_count; };

stmt : assignStmt { $$ = $1; }
     | ifStmt { $$ = $1; }
     ;

assignStmt : ID '=' EXP SEMICOLON {
    $$.next_list.count = 0;
    int id = idx($1);

    char instr[50], temp[50];

    if(id == -1) {
        printf("Error: Variable %s used without declaration.\n", $1);
        accepted = 0;
    }
    else {
        if(!strcmp(sym_tab[1][id], "int")) {
            strcpy(instr, "\tstore i32 ");

            if(!strcmp($3.type, "double") || !strcmp($3.type, "double_const")) 
                printf("Warning: double value assigned to int variable %s\n", sym_tab[0][id]);
            
            if($3.ir_name == -1)
                sprintf(temp, "%d", (int) $3.val);
            else
                sprintf(temp, "%%%d", $3.ir_name);
            
            strcat(instr, temp);
            sprintf(temp, ", i32* %%%d, align 4\n", ir_num[id]);
            strcat(instr, temp);
        } 
        else {
            strcpy(instr, "\tstore double ");
            
            if($3.ir_name == -1) {
                sprintf(temp, "%f", $3.val);
                strcat(temp, "e+00");
            }
            else 
                sprintf(temp, "%%%d", $3.ir_name);

            strcat(instr, temp);
            sprintf(temp, ", double* %%%d, align 8\n", ir_num[id]);
            strcat(instr, temp);
        }
    }
    strcpy(buf[buf_count++], instr);
};

EXP : EXP '+' TERM {
    if(!strcmp($1.type, "int_const") && (!strcmp($3.type, "double") || !strcmp($3.type, "double_const")))
        strcpy($1.type, "double_const");
    if(!strcmp($3.type, "int_const") && (!strcmp($1.type, "double") || !strcmp($1.type, "double_const")))
        strcpy($3.type, "double_const");
    
    $$.ir_name = ir_count++;
    char instr[50], temp[50];
    
    if((!strcmp($1.type, "double") || !strcmp($1.type, "double_const")) && (!strcmp($3.type, "double") || !strcmp($3.type, "double_const"))) {
        strcpy($$.type, "double");
        sprintf(instr, "\t%%%d = fadd double ", $$.ir_name);
        
        if($1.ir_name == -1) {
            sprintf(temp, "%f", $1.val);
            strcat(temp, "e+00");
        }
        else
            sprintf(temp, "%%%d, ", $1.ir_name);
        strcat(instr, temp);
        
        if($3.ir_name == -1) {
            sprintf(temp, "%f", $3.val);
            strcat(temp, "e+00\n");
        }
        else
            sprintf(temp, "%%%d\n", $3.ir_name);
        strcat(instr, temp);
    }
    else if((!strcmp($1.type, "int") || !strcmp($1.type, "int_const")) && (!strcmp($3.type, "int") || !strcmp($3.type, "int_const"))) {
        strcpy($$.type, "int");
        sprintf(instr, "\t%%%d = add nsw i32 ", $$.ir_name);
        
        if($1.ir_name == -1) 
            sprintf(temp, "%d, ", (int) $1.val);
        else
            sprintf(temp, "%%%d, ", $1.ir_name);
        strcat(instr, temp);
        
        if($3.ir_name == -1)
            sprintf(temp, "%d\n", (int) $3.val);
        else
            sprintf(temp, "%%%d\n", $3.ir_name);
        strcat(instr, temp);
    }
    else {
        accepted = 0;
        printf("Error: Typecasting required.\n");
    }
    strcpy(buf[buf_count++], instr);
}

    | EXP '-' TERM {  
    if(!strcmp($1.type, "int_const") && (!strcmp($3.type, "double") || !strcmp($3.type, "double_const")))
        strcpy($1.type, "double_const");
    if(!strcmp($3.type, "int_const") && (!strcmp($1.type, "double") || !strcmp($1.type, "double_const")))
        strcpy($3.type, "double_const");
    
    $$.ir_name = ir_count++;
    char instr[50], temp[50];
    
    if((!strcmp($1.type, "double") || !strcmp($1.type, "double_const")) && (!strcmp($3.type, "double") || !strcmp($3.type, "double_const"))) {
        strcpy($$.type, "double");
        sprintf(instr, "\t%%%d = fsub double ", $$.ir_name);
        
        if($1.ir_name == -1) {
            sprintf(temp, "%f", $1.val);
            strcat(instr, temp);
            strcat(temp, "e+00");
        }
        else
            sprintf(temp, "%%%d, ", $1.ir_name);
        strcat(instr, temp);
        
        if($3.ir_name == -1) {
            sprintf(temp, "%f", $3.val);
            strcat(instr, temp);
            strcat(temp, "e+00");
        }
        else
            sprintf(temp, "%%%d\n", $3.ir_name);
        strcat(instr, temp);
    }
    else if((!strcmp($1.type, "int") || !strcmp($1.type, "int_const")) && (!strcmp($3.type, "int") || !strcmp($3.type, "int_const"))) {
        strcpy($$.type, "int");
        sprintf(instr, "\t%%%d = sub nsw i32 ", $$.ir_name);
        
        if($1.ir_name == -1)
            sprintf(temp, "%d, ", (int) $1.val);
        else
            sprintf(temp, "%%%d, ", $1.ir_name);
        strcat(instr, temp);
        
        if($3.ir_name == -1)
            sprintf(temp, "%d\n", (int) $3.val);
        else
            sprintf(temp, "%%%d\n", $3.ir_name);
        strcat(instr, temp);
    }
    else {
        accepted = 0;
        printf("Error: Typecasting required.\n");
    }
    strcpy(buf[buf_count++], instr);
}

    | TERM {
    $$.ir_name = $1.ir_name;
    $$.val = $1.val;
    strcpy($$.type, $1.type);
};

TERM : TERM '*' FACTOR {
    if(!strcmp($1.type, "int_const") && (!strcmp($3.type, "double") || !strcmp($3.type, "double_const")))
        strcpy($1.type, "double_const");
    if(!strcmp($3.type, "int_const") && (!strcmp($1.type, "double") || !strcmp($1.type, "double_const")))
        strcpy($3.type, "double_const");
    
    $$.ir_name = ir_count++;
    char instr[50], temp[50];
    
    if((!strcmp($1.type, "double") || !strcmp($1.type, "double_const")) && (!strcmp($3.type, "double") || !strcmp($3.type, "double_const"))) {
        strcpy($$.type, "double");
        sprintf(temp, "\t%%%d = fmul double ", $$.ir_name);
        
        if($1.ir_name == -1) {
            sprintf(temp, "%f", $1.val);
            strcat(instr, temp);
            strcat(temp, "e+00");
        }
        else
            sprintf(temp, "%%%d, ", $1.ir_name);
        strcat(instr, temp);
        
        if($3.ir_name == -1) {
            sprintf(temp, "%f", $3.val);
            strcat(instr, temp);
            strcat(temp, "e+00");
        }
        else
            sprintf(temp, "%%%d\n", $3.ir_name);
        strcat(instr, temp);
    }
    else if((!strcmp($1.type, "int") || !strcmp($1.type, "int_const")) && (!strcmp($3.type, "int") || !strcmp($3.type, "int_const"))) {
        strcpy($$.type, "int");
        sprintf(temp, "\t%%%d = mul nsw i32 ", $$.ir_name);
        
        if($1.ir_name == -1)
            sprintf(temp, "%d, ", (int) $1.val);
        else
            sprintf(temp, "%%%d, ", $1.ir_name);
        strcat(instr, temp);
        
        if($3.ir_name == -1)
            sprintf(temp, "%d\n", (int) $3.val);
        else
            sprintf(temp, "%%%d\n", $3.ir_name);
        strcat(instr, temp);
    }
    else {
        accepted = 0;
        printf("Error: Typecasting required.\n");
    }
    strcpy(buf[buf_count++], instr);
}

    | FACTOR {
    $$.ir_name = $1.ir_name;
    $$.val = $1.val;
    strcpy($$.type, $1.type);
};

FACTOR : ID {
    int id = idx($1);
    add_label();
    
    if(id == -1) {
        printf("Error: Variable %s used without declaration.\n", $1);
        accepted = 0;
    }
    else {
        $$.ir_name = ir_count++;

        char instr[50], temp[50];
        sprintf(instr, "\t%%%d = load ", $$.ir_name);
        
        if(!strcmp(sym_tab[1][id], "int")) {
            strcpy($$.type, "int");
            sprintf(temp, "i32, i32* %%%d, align 4\n", ir_num[id]);
        }
        else {
            strcpy($$.type, "double");
            sprintf(temp, "double, double* %%%d, align 8\n", ir_num[id]);
        }
        strcat(instr, temp);
        strcpy(buf[buf_count++], instr);
    }
};

    | INTEGER_CONSTANT {
    add_label();
    $$.val = $1;
    strcpy($$.type, "int_const");
    $$.ir_name = -1;
}

    | DOUBLE_CONSTANT {
    add_label();
    $$.val = $1;
    strcpy($$.type, "double_const");
    $$.ir_name = -1;
};

ifStmt : IF '(' relExp ')' M BEG { start_label = 1; } stmtList END {
    backpatch($3.true_list.id, $3.true_list.offset, $3.true_list.count, $5);
    start_label = 1;

    $$.next_list.count = $3.false_list.count + $8.next_list.count;
    for(int i = 0; i < $3.false_list.count; ++i) {
        $$.next_list.id[i] = $3.false_list.id[i];
        $$.next_list.offset[i] = $3.false_list.offset[i];
    }
    for(int i = 0; i < $8.next_list.count; ++i) {
        $$.next_list.id[i + $3.false_list.count] = $8.next_list.id[i];
        $$.next_list.offset[i + $3.false_list.count] = $8.next_list.offset[i];
    }
};

relExp : EXP '>' EXP {
    strcpy($$.type, "bool");
    $$.ir_name = ir_count++;
    char instr[50], temp[50];
    
    if((!strcmp($1.type, "int") || !strcmp($1.type, "int_const")) && (!strcmp($3.type, "int") || !strcmp($3.type, "int_const"))) {
        sprintf(instr, "\t%%%d = icmp sgt i32 ", $$.ir_name);
    
        if($1.ir_name == -1)
            sprintf(temp, "%d, ", (int) $1.val);
        else
            sprintf(temp, "%%%d, ", $1.ir_name);
        strcat(instr, temp);
        
        if($3.ir_name == -1)
            sprintf(temp, "%d\n", (int) $3.val);
        else
            sprintf(temp, "%%%d\n", $3.ir_name);
        strcat(instr, temp);
    }
    else if((!strcmp($1.type, "double") || !strcmp($1.type, "double_const")) && (!strcmp($3.type, "double") || !strcmp($3.type, "double_const"))) {
        sprintf(instr, "\t%%%d = fcmp ogt double ", $$.ir_name);
        
        if($1.ir_name == -1) {
            sprintf(temp, "%f", $1.val);
            strcat(instr, temp);
            strcat(temp, "e+00");
        }
        else
            sprintf(temp, "%%%d, ", $1.ir_name);
        strcat(instr, temp);
        
        if($3.ir_name == -1) {
            sprintf(temp, "%f", $3.val);
            strcat(instr, temp);
            strcat(temp, "e+00\n");
        }
        else
            sprintf(temp, "%%%d\n", $3.ir_name);
        strcat(instr, temp);
    }
    else {
        accepted = 0;
        printf("Error: Typecasting required.\n");
    }
    strcpy(buf[buf_count++], instr);

    $$.true_list.count = $$.false_list.count = 1;
    $$.true_list.id[0] = buf_count;
    $$.false_list.id[0] = buf_count;

    sprintf(instr, "\tbr i1 %%%d, label %%", $$.ir_name);
    $$.true_list.offset[0] = strlen(instr);
    strcat(instr, "  , label %");
    $$.false_list.offset[0] = strlen(instr);
    strcat(instr, "  \n");
    strcpy(buf[buf_count++], instr);
};
%%

void add_to_sym_tab(char* name) {
    strcpy(sym_tab[0][count], name);
    strcpy(sym_tab[1][count], cur_type);
    ir_num[count++] = ir_count;
}

int idx(char* var) {
    for(int i = 0; i < count; ++i)
        if(!strcmp(var, sym_tab[0][i]))
            return i;
    return -1;
}

void init_lines() {
    sprintf(buf[buf_count++], "; ModuleID = 'a.c'\nsource_filename = \"a.c\"\ntarget datalayout = \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\ntarget triple = \"x86_64-pc-linux-gnu\"\n\n");
}

void ending_lines() {
    sprintf(buf[buf_count++], "attributes #0 = { noinline nounwind optnone uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"=\"false\" \"frame-pointer\"=\"all\" \"less-precise-fpmad\"=\"false\" \"min-legal-vector-width\"=\"0\" \"no-infs-fp-math\"=\"false\" \"no-jump-tables\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+cx8,+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }\n\n!llvm.module.flags = !{!0}\n!llvm.ident = !{!1}\n\n!0 = !{i32 1, !\"wchar_size\", i32 4}\n!1 = !{!\"clang version 10.0.0-4ubuntu1 \"}");
}

void add_alloc() {
    int is_int = !strcmp(cur_type, "int");
    if(is_int)
        sprintf(buf[buf_count++], "\t%%%d = alloca i32, align 4\n", ir_count++);
    else
        sprintf(buf[buf_count++], "\t%%%d = alloca double, align 8\n", ir_count++);
}

void backpatch(int id[], int offset[], int count, int label) {
    char temp[3];
    sprintf(temp, "%d", label);
    int label_len = strlen(temp);

    for(int i = 0; i < count; ++i) 
        for(int j = 0; j < label_len; ++j) 
            buf[id[i]][offset[i] + j] = temp[j];
}

void add_label() {
    if(start_label == 1) {
        start_label = 0;
        if(buf_count == 0 || (buf[buf_count - 1][1] != 'b' || buf[buf_count - 1][2] != 'r'))
            sprintf(buf[buf_count++], "\tbr label %%%d\n", ir_count);
        sprintf(buf[buf_count++], "%d:\n", ir_count++);
    }
}

int main(int argc, char** argv) {
	yyparse();
    fp = fopen("out.ll", "w");
    for(int i = 0; i < buf_count; ++i)
        fprintf(fp, "%s", buf[i]);
    fclose(fp);
}

int yyerror(char* s) {
    fprintf(stderr, "%s\n", s);
}
