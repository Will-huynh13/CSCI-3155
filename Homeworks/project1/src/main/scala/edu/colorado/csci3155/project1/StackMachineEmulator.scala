package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case class LoadIns(s: String) extends StackMachineInstruction // Done
case class StoreIns(s: String) extends StackMachineInstruction // Done
case class PushIns(f: Double) extends StackMachineInstruction // Done
case object AddIns extends StackMachineInstruction //Done
case object SubIns extends StackMachineInstruction //Done
case object MultIns extends StackMachineInstruction //Done
case object DivIns extends StackMachineInstruction //Done
case object ExpIns extends StackMachineInstruction //Done
case object LogIns extends StackMachineInstruction //Done
case object SinIns extends StackMachineInstruction //Done
case object CosIns extends StackMachineInstruction //Done
case object PopIns extends StackMachineInstruction //Done


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double], env: Environment.t, ins: StackMachineInstruction): (List[Double], Environment.t) = {
        //instruction matching
        ins match {

            //LoadIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case LoadIns(s: String) => {
                stack match {
                    case Nil => throw new IllegalArgumentException("Cannot load, stack empty!")
                    case head::restOfList => (restOfList, Environment.extend(s,head,env))// return newStack values and updated Env
                }
            }
            //StoreIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case StoreIns(s: String) => {
                //Using the try and catch thingy
                try{Environment.lookup(s,env)}
                catch{
                    case e: IllegalAccessException => throw new IllegalArgumentException(s"Identifier $s is not in env")
                }
                (Environment.lookup(s,env)::stack,env) // if exist then add to stack
            }
            //PushIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case PushIns(num: Double) => {
                (num :: stack,env) // push the double num to the stack
            }
            //AddIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case AddIns => {
                if(stack.length < 2) { // stack doesn't have 2 elements
                    throw new IllegalArgumentException("Cannot perform AddIns, stack doesn't have enough elements")
                } else {
                    stack match {
                        //Case 1: exists both elements and add under env
                        case v1::v2::restOfList => ((v1+v2)::restOfList,env)
                        // Any other case
                        case _ => throw new IllegalArgumentException("Cannot run AddIns")
                    }
                }
            }
            //SubIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case SubIns => {
                if (stack.length < 2) {
                    throw new IllegalArgumentException("Cannot perform SubIns, stack doesn't have enough elements")
                } else {
                    stack match {
                        // Case where v1 and v2 exist in stack
                        case v1::v2::restOfList => ((v2 - v1)::restOfList,env)
                        // Case will throw an error if at any point there is no more elements in the stack at any moment
                        case _ => throw new IllegalArgumentException("Cannot run SubIns")
                    }
                }
            }
            //MultIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case MultIns => {
                if (stack.length < 2) {
                    throw new IllegalArgumentException("Cannot perform MultIns, stack doesn't have enough elements")
                } else {
                    stack match {
                        // Case where v1 and v2 exist in stack
                        case v1::v2::restOfList => ((v1 * v2)::restOfList,env)
                        // Case will throw an error if at any point there is no more elements in the stack at any moment
                        case _ => throw new IllegalArgumentException("Cannot run MultIns")
                    }
                }
            }
            //DivIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case DivIns => {
                if (stack.length < 2) {
                    throw new IllegalArgumentException("Cannot perform DivIns, stack doesn't have enough elements")
                } else {
                    stack match {
                        // Case where v1 and v2 exist in stack
                        case v1::v2::restOfList => {
                            if(v1 == 0){ // Case we need to check if V1 is 0, if it is then ERROR
                               throw new IllegalArgumentException("Cannot divide by 0!!")
                            } else {
                                ((v2 / v1)::restOfList,env)
                            }
                        }
                        // Case will throw an error if at any point there is no more elements in the stack at any moment
                        case _ => throw new IllegalArgumentException("Cannot run DivIns")
                    }
                }
            }
            //ExpIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case ExpIns => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Cannot perform ExpIns, stack doesn't have enough elements")
                }else {
                    stack match {
                        case v1::restOfList => (math.exp(v1)::restOfList,env) // the popped element gets exp
                        case _ => throw new IllegalArgumentException("Cannot run ExpIns")
                    }
                }
            }
            //LogIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case LogIns => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Cannot perform LogIns, stack doesn't have enough elements")
                }else {
                    stack match {
                        case v1::restOfList => {
                            if (v1 > 0) { // need to check if it is positive
                                (math.log(v1)::restOfList,env) // log if element is > 0
                            }else{
                                throw new IllegalArgumentException("Element is non-positive!")
                            }
                        }
                        case _ => throw new IllegalArgumentException("Cannot run LogIns")
                    }
                }
            }
            //SinIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case SinIns => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Cannot perform SinIns, stack doesn't have enough elements")
                }else {
                    stack match {
                        case v1::restOfList => (math.sin(v1)::restOfList,env) // the popped element gets sin
                        case _ => throw new IllegalArgumentException("Cannot run SinIns") //if the stack is empty during any of the pop ops
                    }
                }
            }
            //CosIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case CosIns => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Cannot perform CosIns, stack doesn't have enough elements")
                }else {
                    stack match {
                        case v1::restOfList => (math.cos(v1)::restOfList,env) // the popped element gets cos
                        case _ => throw new IllegalArgumentException("Cannot run CosIns") //if the stack is empty during any of the pop ops
                    }
                }
            }
            //PopIns logic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            case PopIns => {
                if(stack.isEmpty) { // if the stack is empty!
                    throw new IllegalArgumentException("Cannot perform PopIns, stack doesn't have enough elements")
                }else {
                    stack match {
                        case head::Nil => (Nil,env) // just in case one entry
                        case head::restOfList => (restOfList,env) //more than one
                        case _ => throw new IllegalArgumentException("Cannot run PopIns") //if the stack is empty during any of the pop ops
                    }
                }
            }
        }

    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Environment.t =
        {
            //emulateSingleInstruction(stack: List[Double], env: Environment.t, ins: StackMachineInstruction): (List[Double], Environment.t)
            instructionList.foldLeft[(List[Double],Environment.t)](Nil,Environment.empty)((acc: (List[Double],Environment.t),element: StackMachineInstruction) =>{
                emulateSingleInstruction(acc._1,acc._2,element) //._1 to access the list of doubles, ._2 to access the Env and element is the "stuff"
            })._2 //doing this will let me only return the env.t that the function desires
        }
}