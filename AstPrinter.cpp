#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <./AstNodes.h>
using namespace std;

void printIndent(int depth) {
    for(int i=0; i<depth ; i++) {
        printf("|  ");
    }
    printf("+-");
}

void printString(string str, int depth) {
    printIndent(depth);
    printf("%s\n", str);
}

void printInteger(int value, int depth) {
    printIndent(depth);
    printf("%d\n", value);
}

void printDouble(double value, int depth) {
    printIndent(depth);
    printf("%lf\n", value);
}

void printBinaryExpr(const BinaryExprAST* binaryNode, int depth) {
    if (NULL == binaryNode) {
        fprintf(stderr, "FATAL: binary node is NULL in void printBinaryExpr(BinaryExprAST* binaryNode, int depth)\n");
        exit(1);
    }
    printIndent(depth);
    printf("BinaryExpr\n");
    depth++;
    printExpr(binaryNode->getLeft(), depth);
    printString(binaryNode->getOp(), depth);
    printExpr(binaryNode->getRight(), depth);
}

void printExpr(const ExprAST* exprNode, int depth) {
    switch(exprNode->getType()) {
        case NodeType::IntegerExpr:
            {
                const IntegerExprAST* intNode = static_cast<const IntegerExprAST*>(exprNode);
                printInteger(intNode->getVal(), depth);
            }
            break;
        default:
            fprintf(stderr, "Error in printExpr");
            break;
    }
}