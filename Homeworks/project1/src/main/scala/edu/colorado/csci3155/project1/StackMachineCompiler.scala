package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match {
            //Const rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Const(f) => List(PushIns(f))
            //Ident rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Ident(s) => List(StoreIns(s))
            //Plus rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Plus(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(AddIns)
            //Minus rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Minus(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(SubIns)
            //Mult rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Mult(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(MultIns)
            //Div rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Div(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(DivIns)
            //Log rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Log(e1) => compileToStackMachineCode(e1) ++ List(LogIns)
            //Exp rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Exp(e1) => compileToStackMachineCode(e1) ++ List(ExpIns)
            //Sine rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Sine(e1) => compileToStackMachineCode(e1) ++ List(SinIns)
            //Cosine rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Cosine(e1) => compileToStackMachineCode(e1) ++ List(CosIns)
            //Let rule~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case Let(identifier, e1, e2) =>  compileToStackMachineCode(e1) ++ List(LoadIns(identifier)) ++ compileToStackMachineCode(e2)
        }
    }
}
