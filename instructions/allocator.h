#ifndef ALLOCATOR_H
#define ALLOCATOR_H

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
// todo: make this code more readable because like wtf
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          const std::string &VarName)
{

    // creates an IRBuilder object at the first instrcution of the entry block
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                     TheFunction->getEntryBlock().begin());

    // create alloca with the expected name and return it
    return TmpB.CreateAlloca(Type::getDoubleTy(*TheContext), nullptr,
                             VarName);
}

#endif