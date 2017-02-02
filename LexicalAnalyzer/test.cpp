#include "lexical_analyzer.h"
#include "../SyntaxAnalyzer/syntax_analyzer.h"
#include <iostream>
#include <vector>

int main()
{
    std::string basic = u8"int name";
    std::string ptr = u8"int* name";
    std::string function = u8"int main(int arg, char** argv)";
    std::string functionPtr = u8"int (*main)()";

    mq::lexical_analyzer lex{function};
    lex.initialize();
    mq::syntax_analyzer syn{lex};

}
