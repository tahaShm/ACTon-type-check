package main.visitor;

import main.ast.node.Main;
import main.ast.node.Program;
import main.ast.node.declaration.ActorDeclaration;
import main.ast.node.declaration.ActorInstantiation;
import main.ast.node.declaration.VarDeclaration;
import main.ast.node.declaration.handler.HandlerDeclaration;
import main.ast.node.declaration.handler.MsgHandlerDeclaration;
import main.ast.node.expression.*;
import main.ast.node.expression.operators.UnaryOperator;
import main.ast.node.expression.values.BooleanValue;
import main.ast.node.expression.values.IntValue;
import main.ast.node.expression.values.StringValue;
import main.ast.node.expression.values.Value;
import main.ast.node.statement.*;
import main.ast.type.Type;
import main.ast.type.actorType.ActorType;
import main.ast.type.arrayType.ArrayType;
import main.ast.type.noType.NoType;
import main.ast.type.primitiveType.BooleanType;
import main.ast.type.primitiveType.IntType;
import main.ast.type.primitiveType.StringType;
import main.symbolTable.SymbolTable;
import main.symbolTable.SymbolTableActorItem;
import main.symbolTable.SymbolTableHandlerItem;
import main.symbolTable.SymbolTableMainItem;
import main.symbolTable.itemException.ItemAlreadyExistsException;
import main.symbolTable.itemException.ItemNotFoundException;
import main.symbolTable.symbolTableVariableItem.SymbolTableActorVariableItem;
import main.symbolTable.symbolTableVariableItem.SymbolTableKnownActorItem;
import main.symbolTable.symbolTableVariableItem.SymbolTableLocalVariableItem;
import main.symbolTable.symbolTableVariableItem.SymbolTableVariableItem;

import java.util.ArrayList;

public class typeCheckerImpl implements Visitor {
    SymbolTable currentSymbolTable =  null;
    SymbolTable currentActorTable =  null;
    int inLoop = 0;
    Boolean inInitial = false;
    protected void visitStatement( Statement stat )
    {
        if( stat == null )
            return;
        else if( stat instanceof MsgHandlerCall)
            this.visit( ( MsgHandlerCall ) stat );
        else if( stat instanceof Block)
            this.visit( ( Block ) stat );
        else if( stat instanceof Conditional)
            this.visit( ( Conditional ) stat );
        else if( stat instanceof For)
            this.visit( ( For ) stat );
        else if( stat instanceof Break )
            this.visit( ( Break ) stat );
        else if( stat instanceof Continue )
            this.visit( ( Continue ) stat );
        else if( stat instanceof Print )
            this.visit( ( Print ) stat );
        else if( stat instanceof Assign )
            this.visit( ( Assign ) stat );
    }

    protected void visitExpr( Expression expr )
    {
        if( expr == null )
            return;
        else if( expr instanceof UnaryExpression)
            this.visit( ( UnaryExpression ) expr );
        else if( expr instanceof BinaryExpression)
            this.visit( ( BinaryExpression ) expr );
        else if( expr instanceof ArrayCall)
            this.visit( ( ArrayCall ) expr );
        else if( expr instanceof ActorVarAccess)
            this.visit( ( ActorVarAccess ) expr );
        else if( expr instanceof Identifier )
            this.visit( ( Identifier ) expr );
        else if( expr instanceof Self )
            this.visit( ( Self ) expr );
        else if( expr instanceof Sender )
            this.visit( ( Sender ) expr );
        else if( expr instanceof BooleanValue)
            this.visit( ( BooleanValue ) expr );
        else if( expr instanceof IntValue)
            this.visit( ( IntValue ) expr );
        else if( expr instanceof StringValue)
            this.visit( ( StringValue ) expr );
    }

    @Override
    public void visit(Program program) {
        if (program.getActors() != null) {
            for (ActorDeclaration actorDeclaration : program.getActors()) {
                actorDeclaration.accept(this);
            }
        }

        if (program.getMain() != null)
            program.getMain().accept(this);
    }

    @Override
    public void visit(ActorDeclaration actorDeclaration) {
        SymbolTableActorItem newSymbolActorItem = null;
        try {
            newSymbolActorItem = (SymbolTableActorItem)(SymbolTable.root.get(SymbolTableActorItem.STARTKEY + actorDeclaration.getName().getName()));
        }
        catch (ItemNotFoundException e){
            System.out.println("error in actor declaration");
        }
        if (newSymbolActorItem != null)
            currentActorTable = newSymbolActorItem.getActorSymbolTable();


        if (actorDeclaration.getParentName() != null) {
            try {
                currentActorTable.get(SymbolTableActorItem.STARTKEY + actorDeclaration.getParentName().getName());
            }
            catch (ItemNotFoundException e) {
                System.out.println("parent not defined");
            }
        }

        if (actorDeclaration.getKnownActors() != null) {
            for (VarDeclaration varDeclaration : actorDeclaration.getKnownActors()) {
//                varDeclaration.accept(this);
                try {
                    currentActorTable.get(SymbolTableActorItem.STARTKEY + varDeclaration.getType().toString());
                }
                catch (ItemNotFoundException e) {
                    System.out.println("known actor not defined");
                }

            }
        }

        if (actorDeclaration.getActorVars() != null) {
            for (VarDeclaration varDeclaration : actorDeclaration.getActorVars()) {
                varDeclaration.accept(this);
            }
        }

        if (actorDeclaration.getInitHandler() != null) {
            inInitial = true;
            actorDeclaration.getInitHandler().accept(this);
            inInitial = false;
        }

        if (actorDeclaration.getMsgHandlers() != null) {
            for (MsgHandlerDeclaration msgHandlerDeclaration : actorDeclaration.getMsgHandlers()) {
                msgHandlerDeclaration.accept(this);
            }
        }

    }

    @Override
    public void visit(HandlerDeclaration handlerDeclaration) {
        SymbolTableHandlerItem newSymbolHandleItem = null;
        try {
            newSymbolHandleItem = (SymbolTableHandlerItem)(currentActorTable.get(SymbolTableHandlerItem.STARTKEY + handlerDeclaration.getName().getName()));
        }
        catch (ItemNotFoundException e){
            System.out.println("error in handler");
        }
        if (newSymbolHandleItem != null)
            currentSymbolTable = newSymbolHandleItem.getHandlerSymbolTable();

        if (handlerDeclaration.getBody() != null) {
            for (Statement statement : handlerDeclaration.getBody()) {
                if (statement != null)
                    statement.accept(this);
            }
        }
    }

    @Override
    public void visit(VarDeclaration varDeclaration) {
    }

    @Override
    public void visit(Main mainActors) {
        SymbolTableMainItem currentSymbolMainItem = null;
        try {
            currentSymbolMainItem = (SymbolTableMainItem)(SymbolTable.root.get(SymbolTableMainItem.STARTKEY + "main"));
        }
        catch (ItemNotFoundException e){
            System.out.println("error in main");
        }
        if (currentSymbolMainItem != null)
            currentSymbolTable = currentSymbolMainItem.getMainSymbolTable();

        if (mainActors.getMainActors() != null) {
            for (ActorInstantiation actorInstantiation : mainActors.getMainActors()) {
                actorInstantiation.accept(this);
            }
        }
    }

    @Override
    public void visit(ActorInstantiation actorInstantiation) {
        SymbolTableActorItem currentActorItem = null;
        ActorDeclaration currentActorDec = null;
        SymbolTableVariableItem currentKnownActor = null;
        ArrayList<VarDeclaration> knownActors;
        if (actorInstantiation.getIdentifier() != null) {

            try {
                currentActorItem = (SymbolTableActorItem)currentSymbolTable.get(SymbolTableActorItem.STARTKEY + actorInstantiation.getType().toString());
            }
            catch (ItemNotFoundException e) {
                System.out.println("actor instantiation not defined");
            }
        }

        if (currentActorItem != null)
            currentActorDec = currentActorItem.getActorDeclaration();

        if (currentActorDec != null) {
            knownActors = currentActorDec.getKnownActors();
            if (actorInstantiation.getKnownActors().size() != knownActors.size()) {
                System.out.println("different numbers in actor instantiation");
            } else {
                int knownActorIndex = 0;
                Boolean errorInKnownActor = false;
                if (actorInstantiation.getKnownActors() != null) {
                    for (Identifier identifier : actorInstantiation.getKnownActors()) {
                        identifier.accept(this);

                        try {
                            currentKnownActor = (SymbolTableVariableItem) (currentSymbolTable.get(SymbolTableVariableItem.STARTKEY + identifier.getName()));
                        }
                        catch (ItemNotFoundException e) { }


                        if (!currentKnownActor.getVarDeclaration().getType().toString().equals(knownActors.get(knownActorIndex).getType().toString())) {
                            errorInKnownActor = true;
                        }
                        knownActorIndex++;
                    }
                    if (errorInKnownActor)
                        System.out.println("type mismatch in actor instantiation");
                }
            }
        }

//        if (actorInstantiation.getInitArgs() != null) {
//            for (Expression expression : actorInstantiation.getInitArgs()) {
//                expression.accept(this);
//            }
//        }
    }

    @Override
    public void visit(UnaryExpression unaryExpression) {
        if (unaryExpression.getOperand() != null) {
            unaryExpression.getOperand().accept(this);
            String operator = unaryExpression.getUnaryOperator().toString();
            if (operator == "preinc" || operator == "postinc" || operator == "predec" || operator == "postDec"){
                if (!(unaryExpression.getOperand() instanceof Identifier) && !(unaryExpression.getOperand() instanceof ActorVarAccess)) {
                    System.out.println("expression of -- or ++ must be lValue");
                }
            }
            Type expType = expressionType(unaryExpression);
            unaryExpression.setType(expType);
            if (expType instanceof NoType){
                System.out.println("error in unary operation");
            }
            else{

            }
        }
    }

    @Override
    public void visit(BinaryExpression binaryExpression) {
        if(binaryExpression.getLeft() != null)
            binaryExpression.getLeft().accept(this);

        if(binaryExpression.getRight() != null)
            binaryExpression.getRight().accept(this);

        Type expType = expressionType(binaryExpression);
        binaryExpression.setType(expType);
        if (expType instanceof NoType){
            System.out.println("error in binary operation");
        }
        else{

        }


    }

    @Override
    public void visit(ArrayCall arrayCall) {
        if (arrayCall.getArrayInstance() != null)
            arrayCall.getArrayInstance().accept(this);
    }

    @Override
    public void visit(ActorVarAccess actorVarAccess) {

        if (actorVarAccess.getSelf() != null) {
            actorVarAccess.getSelf().accept(this);
        }

        if (actorVarAccess.getVariable() != null) {
            SymbolTableVariableItem varDec = null;
            try {
                varDec = (SymbolTableVariableItem) (currentActorTable.get(SymbolTableVariableItem.STARTKEY + actorVarAccess.getVariable().getName()));
            }
            catch (ItemNotFoundException e) {
                System.out.println("variable used in self not defined");
            }
        }
    }

    @Override
    public void visit(Identifier identifier) {
        SymbolTableVariableItem varDec = null;
        try {
            varDec = (SymbolTableVariableItem) (currentSymbolTable.get(SymbolTableVariableItem.STARTKEY + identifier.getName()));
        }
        catch (ItemNotFoundException e){
            System.out.println("variable not defined");
        }

    }

    @Override
    public void visit(Self self) {
    }

    @Override
    public void visit(Sender sender) {
        if (inInitial)
            System.out.println("sender used in initial");
    }

    @Override
    public void visit(BooleanValue value) {
    }

    @Override
    public void visit(IntValue value) {
    }

    @Override
    public void visit(StringValue value) {
    }

    @Override
    public void visit(Block block) {
        if (block.getStatements() != null) {
            for (Statement statement : block.getStatements()) {
                statement.accept(this);
            }
        }
    }

    @Override
    public void visit(Conditional conditional) {
        if (conditional.getExpression() != null) {
            conditional.getExpression().accept(this);
            if (!(expressionType(conditional.getExpression()) instanceof BooleanType) && !(expressionType(conditional.getExpression()) instanceof NoType))
                System.out.println("Error in if condition");
        }

        if (conditional.getThenBody() != null)
            conditional.getThenBody().accept(this);

        if (conditional.getElseBody() != null)
            conditional.getElseBody().accept(this);
    }

    @Override
    public void visit(For loop) {
        inLoop += 1;
        if (loop.getInitialize() != null)
            loop.getInitialize().accept(this);

        if (loop.getCondition() != null) {
            loop.getCondition().accept(this);
            if (!(expressionType(loop.getCondition()) instanceof BooleanType) && !(expressionType(loop.getCondition()) instanceof NoType))
                System.out.println("Error in for condition");
        }

        if (loop.getUpdate() != null)
            loop.getUpdate().accept(this);

        if (loop.getBody() != null)
            loop.getBody().accept(this);

        inLoop -= 1;
    }

    @Override
    public void visit(Break breakLoop) {
        if (inLoop < 1)
            System.out.println("break not in loop");
    }

    @Override
    public void visit(Continue continueLoop) {
        if (inLoop < 1)
            System.out.println("continue not in loop");
    }

    @Override
    public void visit(MsgHandlerCall msgHandlerCall) {
        if (msgHandlerCall.getInstance() != null) {
            msgHandlerCall.getInstance().accept(this);
            SymbolTableVariableItem inst = null;
            try {
                if (!(msgHandlerCall.getInstance() instanceof Sender || msgHandlerCall.getInstance() instanceof Self))
                    inst = (SymbolTableVariableItem) currentActorTable.get(SymbolTableVariableItem.STARTKEY + ((Identifier) msgHandlerCall.getInstance()).getName());
            }
            catch (ItemNotFoundException e) { }
            if (!(inst instanceof SymbolTableKnownActorItem) && !(msgHandlerCall.getInstance() instanceof Sender) && !(msgHandlerCall.getInstance() instanceof Self)) {
                System.out.println("actor var not found (in msgHandler)");
            }
        }

        if (msgHandlerCall.getMsgHandlerName() != null) {
            SymbolTableKnownActorItem currentKnownActorItem = null;
            SymbolTableActorItem currentActorItem = null;
            SymbolTableHandlerItem currentHandler = null;
            Type currentType = null;
            SymbolTable currentActor = null;
            try {
                if (!(msgHandlerCall.getInstance() instanceof Self))
                    currentKnownActorItem = (SymbolTableKnownActorItem)(currentActorTable.get(SymbolTableKnownActorItem.STARTKEY + ((Identifier) msgHandlerCall.getInstance()).getName()));
            }

            catch (ItemNotFoundException e){}
            if (msgHandlerCall.getInstance() instanceof Self) {
                Identifier id = new Identifier(currentActorTable.getName());
                Type curType = new ActorType(id);
                currentType = curType;
            }
            else if (currentKnownActorItem != null) {
                currentType = currentKnownActorItem.getType();


                try {
                    currentActorItem = (SymbolTableActorItem) (SymbolTable.root.get(SymbolTableActorItem.STARTKEY + currentType.toString()));
                } catch (ItemNotFoundException e) {
                }

                if (currentActorItem != null)
                    currentActor = currentActorItem.getActorSymbolTable();

                try {
                    currentHandler = (SymbolTableHandlerItem) (currentActor.get(SymbolTableHandlerItem.STARTKEY + ((Identifier) msgHandlerCall.getMsgHandlerName()).getName()));
                } catch (ItemNotFoundException e) {
                    System.out.println("msg handler does not exist");
                }
            }
            else {
                System.out.println("msg handler does not exist");
            }
        }

        if (msgHandlerCall.getArgs() != null) {
            for (Expression expression : msgHandlerCall.getArgs()) {
                expression.accept(this);
            }
        }
    }

    @Override
    public void visit(Print print) {

        if (print.getArg() != null) {
            print.getArg().accept(this);
            Type argType = expressionType(print.getArg());
            if (!(argType instanceof NoType) && !(argType instanceof IntType) && !(argType instanceof BooleanType) && !(argType instanceof ArrayType) && !(argType instanceof StringType)){
                System.out.println("error in print argument");
            }
        }


    }

    @Override
    public void visit(Assign assign) {
        Type rType = null;
        Type lType = null;
        SymbolTableVariableItem actorVaraccess = null;

        if (assign.getlValue() != null) {
            if (!(assign.getlValue() instanceof Identifier) && !(assign.getlValue() instanceof ActorVarAccess)) {
                System.out.println("Lvalue must be identifier or actor var access");
            }
            assign.getlValue().accept(this);
            lType = expressionType(assign.getlValue());
        }

        if (assign.getrValue() != null) {
            assign.getrValue().accept(this);
            rType = expressionType(assign.getrValue());
        }
        if (rType.toString() != lType.toString() && (rType.toString() != "notype" && lType.toString() != "notype"))
            System.out.println("Error in assign");
    }


    public Type expressionType(Expression expression) {
        Type returnType = null;
        SymbolTableVariableItem actorVaraccess = null;


        if (expression instanceof UnaryExpression) {
            Expression operand = ((UnaryExpression) expression).getOperand();
            String operator = ((UnaryExpression) expression).getUnaryOperator().toString();
            if (operator == "minus" || operator == "preinc" || operator == "postinc" || operator == "predec" || operator == "postDec") {
                if (expressionType(operand) instanceof IntType)
                    returnType = new IntType();
                else
                    returnType = new NoType();
            }
            else if (operator == "not") {
                if (expressionType(operand) instanceof BooleanType)
                    returnType = new BooleanType();
                else
                    returnType = new NoType();
            }
        }


        else if (expression instanceof BinaryExpression) {
            Expression lOperand = ((BinaryExpression) expression).getLeft();
            Expression rOperand = ((BinaryExpression) expression).getRight();
            String operator = ((BinaryExpression) expression).getBinaryOperator().toString();
            if (operator == "add" || operator == "sub" || operator == "mult" || operator == "div" || operator == "mod") {
                if (expressionType(lOperand) instanceof IntType && expressionType(rOperand) instanceof IntType)
                    returnType = new IntType();
                else
                    returnType = new NoType();
            }
            else if (operator == "gt" || operator == "lt") {
                if (expressionType(lOperand) instanceof IntType && expressionType(rOperand) instanceof IntType)
                    returnType = new BooleanType();
                else
                    returnType = new NoType();
            }
            else if (operator == "and" || operator == "or") {
                if (expressionType(lOperand) instanceof BooleanType && expressionType(rOperand) instanceof BooleanType)
                    returnType = new BooleanType();
                else
                    returnType = new NoType();
            }
            else if (operator == "assign") {
                if (expressionType(lOperand).toString() == expressionType(rOperand).toString())
                    returnType = expressionType(lOperand);
                else
                    returnType = new NoType();
            }
            else if (operator == "eq" || operator == "neq") {
                if (expressionType(lOperand).toString() == expressionType(rOperand).toString())
                    returnType = new BooleanType();
                else
                    returnType = new NoType();
            }
        }


        else if (expression instanceof Identifier) {
            try {
                SymbolTableVariableItem idItem = (SymbolTableVariableItem)currentSymbolTable.get(SymbolTableVariableItem.STARTKEY + ((Identifier) expression).getName());
                returnType = idItem.getType();
            }
            catch (ItemNotFoundException e) {
                returnType = new NoType();
            }
        }


        else if (expression instanceof Value)
            returnType = expression.getType();


        else if (expression instanceof ActorVarAccess) {
            try {
                actorVaraccess = (SymbolTableVariableItem) currentActorTable.get(SymbolTableVariableItem.STARTKEY + ((ActorVarAccess) expression).getVariable().getName());
            }
            catch (ItemNotFoundException e) {
                returnType = new NoType();
                return returnType;
            }
            returnType = actorVaraccess.getType();
        }
        return returnType;
    }
}
