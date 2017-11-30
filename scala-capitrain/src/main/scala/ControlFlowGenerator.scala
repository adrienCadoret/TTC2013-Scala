import java.io.IOException
import java.util

import org.eclipse.emf.common.util.{EList, TreeIterator, URI}
import org.eclipse.emf.ecore.resource.{Resource, ResourceSet}
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import org.eclipse.emf.ecore.xmi.impl.{EcoreResourceFactoryImpl, XMIResourceFactoryImpl}
import org.eclipse.emf.ecore.{EAttribute, EClass, EFactory, EObject, EPackage, EReference, EStructuralFeature}
import org.eclipse.emf.ecore.xmi.XMIResource
import java.io.IOException

object ControlFlowGenerator extends App {


  val resourceSet : ResourceSet  = new ResourceSetImpl()

  createFlowGraphModelInstance(loadFlowGraphMetaModel(),"./Test1.xmi" , "./test.xmi")

  def loadFlowGraphMetaModel(): Resource = {
    resourceSet.getResourceFactoryRegistry.getExtensionToFactoryMap.put("ecore", new EcoreResourceFactoryImpl())
    resourceSet.getResourceFactoryRegistry.getExtensionToFactoryMap.put("xmi", new XMIResourceFactoryImpl())
    val flowGraphMetalModel = resourceSet.getResource(URI.createFileURI("FlowGraph.ecore"), true)
    flowGraphMetalModel
  }

  def isFlowInstruction(eObject: EObject) = {
    var res = false
    eObject.eClass().getEAllSuperTypes.forEach(classe => {
      if(classe.getName.equals("FlowInstr"))
        res = true
    })
    res
  }

  def getNextInstruction(resource : Resource, eObject: EObject) : EObject= {
    val contentsTree = resource.getAllContents
    var nextInstrustionObject : EObject = null
    while (contentsTree.hasNext()) {
      var node = contentsTree.next()
      if(node == eObject){
        contentsTree.prune()
        nextInstrustionObject = contentsTree.next()
      }
    }
    nextInstrustionObject
  }

  def getExitInstruction(resource : Resource) : EObject= {
    val contentsTree = resource.getAllContents
    var exitInstrustionObject : EObject = null
    while (contentsTree.hasNext()) {
      var node = contentsTree.next()
      if(node.eClass().getName == "Exit"){
        exitInstrustionObject = node
      }
    }
    exitInstrustionObject
  }

  def getFirstInstructionOfObject(eObject: EObject) = {
    val contentsTree = eObject.eAllContents()
    var instruction : EObject = null
    while (contentsTree.hasNext()) {
      var node = contentsTree.next()
      if(instruction == null && isFlowInstruction(node)){
        instruction = node
      }

    }
    instruction
  }

  def getContainerWithName(node: EObject, typeName: String) = {
    var res : EObject = null
    var currentContainer = node.eContainer()
    while (!(currentContainer == null | res != null)) {
      if(currentContainer.eClass().getName.equals(typeName))
        res = currentContainer
      currentContainer = currentContainer.eContainer()
    }
    res
  }

  def generateFlowInstructionRelations(resource: Resource, tree : TreeIterator[EObject], previousNode : EObject) : Any = {
    while (tree.hasNext()) {
      var node = tree.next()
      node.eClass().getName match {
        case "SimpleStmt" => {
          node.eContainer().eClass().getName match {
            case "Method" => {
              previousNode.eGet(previousNode.eClass().getEStructuralFeature("cfNext")).asInstanceOf[util.Collection[EObject]].add(node)
              node.eGet(node.eClass().getEStructuralFeature("cfPrev")).asInstanceOf[util.Collection[EObject]].add(previousNode)
            }
            case "Block" => {
              var loop = getContainerWithName(node, "Loop")
              if(loop != null){
                var firstInstructionInLoop = getFirstInstructionOfObject(loop)
                node.eGet(node.eClass().getEStructuralFeature("cfNext")).asInstanceOf[util.Collection[EObject]].add(firstInstructionInLoop)
                firstInstructionInLoop.eGet(firstInstructionInLoop.eClass().getEStructuralFeature("cfPrev")).asInstanceOf[util.Collection[EObject]]
              }
            }
          }
        }
        case "Expr" => {
          node.eContainer().eClass().getName match {
            case "Loop" => {
              var nextInstructionOutsideLoop = getNextInstruction(resource, node.eContainer())
              node.eGet(node.eClass().getEStructuralFeature("cfNext")).asInstanceOf[util.Collection[EObject]].add(nextInstructionOutsideLoop)
              nextInstructionOutsideLoop.eGet(nextInstructionOutsideLoop.eClass().getEStructuralFeature("cfPrev")).asInstanceOf[util.Collection[EObject]].add(node)

              var bodyOfLoop = getObjectByStructuralFeatureName(node.eContainer(), "body")
              var firstInstructionInsideLoop = getFirstInstructionOfObject(bodyOfLoop)
              node.eGet(node.eClass().getEStructuralFeature("cfNext")).asInstanceOf[util.Collection[EObject]].add(firstInstructionInsideLoop)
              firstInstructionInsideLoop.eGet(firstInstructionInsideLoop.eClass().getEStructuralFeature("cfPrev")).asInstanceOf[util.Collection[EObject]]
            }
            case "If" => {
              var thenOfIf = getObjectByStructuralFeatureName(node.eContainer(), "then")
              var firstInstructionInsideThen = getFirstInstructionOfObject(thenOfIf)
              node.eGet(node.eClass().getEStructuralFeature("cfNext")).asInstanceOf[util.Collection[EObject]].add(firstInstructionInsideThen)
              firstInstructionInsideThen.eGet(firstInstructionInsideThen.eClass().getEStructuralFeature("cfPrev")).asInstanceOf[util.Collection[EObject]]

              var elseOfIf = getObjectByStructuralFeatureName(node.eContainer(), "else")
              if(elseOfIf != null){
                var firstInstructionInsideElse = getFirstInstructionOfObject(elseOfIf)
                node.eGet(node.eClass().getEStructuralFeature("cfNext")).asInstanceOf[util.Collection[EObject]].add(firstInstructionInsideElse)
                firstInstructionInsideElse.eGet(firstInstructionInsideThen.eClass().getEStructuralFeature("cfPrev")).asInstanceOf[util.Collection[EObject]]
              }
            }
          }
        }
        case "Return" => {
          val exitObject = getExitInstruction(resource)
          node.eGet(node.eClass().getEStructuralFeature("cfNext")).asInstanceOf[util.Collection[EObject]].add(exitObject)
          exitObject.eGet(exitObject.eClass().getEStructuralFeature("cfPrev")).asInstanceOf[util.Collection[EObject]].add(node)
        }
        case "Break" => {
          var loop = getContainerWithName(node, "Loop")
          if(loop != null){
            var firstInstructionOutsideLoop = getNextInstruction(resource, loop)
            node.eGet(node.eClass().getEStructuralFeature("cfNext")).asInstanceOf[util.Collection[EObject]].add(firstInstructionOutsideLoop)
            firstInstructionOutsideLoop.eGet(firstInstructionOutsideLoop.eClass().getEStructuralFeature("cfPrev")).asInstanceOf[util.Collection[EObject]]
          }
        }
        case default =>
      }
      generateFlowInstructionRelations(resource, tree, node)
      tree.prune()
    }
    null
  }

  def createFlowGraphModelInstance(metaModel : Resource, uriSource: String, createdFile: String) = {
    // Creating ressource
    val resource : Resource  = resourceSet.createResource(URI.createURI(createdFile))
    // Getting the right package ,  the only one
    val ePackage = metaModel.getContents.get(0).asInstanceOf[EPackage]
    // Creating a factory to generate an instance of the metamodel
    val flowGraphFactoryInstance : EFactory  = ePackage.getEFactoryInstance()

    // Loop on the XMI in input to generate an instance of the model
    generateModel(resource, resourceSet.getResource(URI.createURI(uriSource), true).getAllContents, ePackage, flowGraphFactoryInstance, null)

    // Looping on generated XMI to create relations between elements (cfNext and cfPrev)
    generateFlowInstructionRelations(resource, resource.getAllContents, null)

    // Options for the output file
    val options = new util.HashMap[String, Boolean]()
    options.put(XMIResource.XMI_URI, true);
    try
      resource.save(options)
    catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }

  def generateExit(ePackage: EPackage, flowGraphFactoryInstance: EFactory, eObject: EObject) = {
    val exitEObject : EObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("Exit").asInstanceOf[EClass])
    val exitAttribute : EAttribute  = getAttributeByName(eObject, "txt")
    exitEObject.eSet(exitAttribute, "Exit")
    setObjectToContainer(exitEObject, eObject, "exit")
  }

  def generateModel(resource : Resource, tree : TreeIterator[EObject], ePackage: EPackage, flowGraphFactoryInstance : EFactory,
                    containerObject : EObject) : Void = {
    while (tree.hasNext()) {
      val node = tree.next()
      node.eClass().getName match {
        case "ClassMethod" => {
          val eObject: EObject = compileMethod(resource, ePackage, flowGraphFactoryInstance, node)
          generateModel(resource, node.eAllContents(), ePackage, flowGraphFactoryInstance, eObject)
          generateExit(ePackage, flowGraphFactoryInstance, eObject)
        }
        case "LocalVariableStatement" => {
          val eObject: EObject = compileLocalVariableStatement(ePackage, flowGraphFactoryInstance, node, containerObject)
          generateModel(resource, node.eAllContents(), ePackage, flowGraphFactoryInstance, eObject)
        }
        case "ExpressionStatement" => {
          val eObject : EObject = compileExpressionStatement(ePackage, flowGraphFactoryInstance, node, containerObject)
          generateModel(resource, node.eAllContents(), ePackage, flowGraphFactoryInstance, eObject)
        }
        case "Return" => {
          val returnEObject: EObject = compileReturn(ePackage, flowGraphFactoryInstance, node, containerObject)
          generateModel(resource, node.eAllContents(), ePackage, flowGraphFactoryInstance, returnEObject)
        }
        case "WhileLoop" => {
          val whileLoopObject: EObject = compileWhileLoop(ePackage, flowGraphFactoryInstance, node, containerObject)
          tree.prune()
          generateModel(resource, node.eAllContents(), ePackage, flowGraphFactoryInstance, whileLoopObject)
        }
        case default => generateModel(resource, node.eAllContents(), ePackage, flowGraphFactoryInstance, null)
      }
      tree.prune()
    }
    null
  }

  def compileMethod(resource: Resource, ePackage: EPackage, flowGraphFactoryInstance: EFactory, node: EObject) : EObject = {
    val methodEClass = ePackage.getEClassifier("Method").asInstanceOf[EClass]
    val methodEObject = flowGraphFactoryInstance.create(methodEClass)
    val classMethodNameAttribute : EAttribute  = node.eClass.getEAllAttributes().get(0)
    val txtAttribute : EAttribute = methodEClass.getEAllAttributes().get(0)
    methodEObject.eSet(txtAttribute, node.eGet(classMethodNameAttribute))
    resource.getContents.add(methodEObject)
    methodEObject
  }

  def getAttributeByName(eObject : EObject, attributeName: String) : EAttribute = {
    var res : EAttribute = null
    eObject.eClass.getEAllAttributes.forEach(eAtt => {
      if(eAtt.getName.equals(attributeName)) res = eAtt
    })
    res
  }

  def getReferenceByName(eObject : EObject, referenceName: String) : EReference = {
    var res : EReference = null
    val test = eObject.eClass.getEReferences
    eObject.eClass.getEReferences.forEach(eRef => {
      if(eRef.getName.equals(referenceName)) res = eRef
    })
    res
  }

  def getObjectByStructuralFeatureName(node : EObject, structuralFeatureName: String) : EObject = {
    node.eGet(node.eClass.getEStructuralFeature(structuralFeatureName)).asInstanceOf[EObject]
  }

  def getListObjectsByStructuralFeatureName(node : EObject, structuralFeatureName: String) : util.List[EObject] = {
    node.eGet(node.eClass.getEStructuralFeature(structuralFeatureName)).asInstanceOf[util.List[EObject]]
  }

  def getCrossReferenceObject(element: EObject) : EObject = {
    if(element.eCrossReferences().size() > 0){
      return element.eCrossReferences().get(0)
    }
    null
  }

  def getOperator(operatorClassName: String) : String = {
    operatorClassName match {
      case "Addition" => return "+"
      case "Multiplication" => return "*"
      case "Division" => return "/"
      case "GreaterThan" => return ">"
      case "LessThan" => return "<"
      case "Subtraction" => return "-"
      case "Equal" => return "=="
      case "Assignment" => return "="
      case "MinusMinus" => return "--"
      case "PlusPlus" => return "++"
    }
  }

  def getValueOf(eObject: EObject) : String = {
    var res = ""
    eObject.eClass().getName match {
      case "IdentifierReference" => {
        if(eObject.eCrossReferences().size() == 1) {
          val crossRefObject : EObject = getCrossReferenceObject(eObject)
          res = crossRefObject.eGet(getAttributeByName(crossRefObject, "name")).toString
        }
      }
      case "DecimalIntegerLiteral" => {
        res = eObject.eGet(getAttributeByName(eObject, "decimalValue")).toString
      }
      case "AdditiveExpression" | "MultiplicativeExpression" => {
        var childrens = new util.ArrayList[String]()
        var operator = ""
        eObject.eContents().forEach(element => {
          element.eClass.getName match {
            case "IdentifierReference" => {
              if(element.eCrossReferences().size() == 1) {
                val crossRefObject : EObject = getCrossReferenceObject(element)
                childrens.add(crossRefObject.eGet(getAttributeByName(crossRefObject, "name")).toString)
              }
            }
            case "Addition" | "Multiplication" | "Division" | "Subtraction" => {
              operator = getOperator(element.eClass.getName)
            }
            case "DecimalIntegerLiteral" => {
              childrens.add(element.eGet(getAttributeByName(element, "decimalValue")).toString)
            }
          }
        })
        res = childrens.get(0) + " " + operator + " " + childrens.get(1)
      }
      case "AssignmentExpression" => {
        var child : String = null
        var assignmentOperator : String = null
        var value : String = null

        eObject.eClass().getEAllStructuralFeatures.forEach(structuralFeature => {
          structuralFeature.getName match {
            case "child" => {
              if(getObjectByStructuralFeatureName(eObject, "child").eCrossReferences().size() == 1) {
                val crossRefObject : EObject = getCrossReferenceObject(getObjectByStructuralFeatureName(eObject, "child"))
                child = crossRefObject.eGet(getAttributeByName(crossRefObject, "name")).toString
              }
            }
            case "assignmentOperator" => {
              assignmentOperator = getOperator(getObjectByStructuralFeatureName(eObject, "assignmentOperator").eClass.getName)
            }
            case "value" => {
              value = getValueOf(getObjectByStructuralFeatureName(eObject, "value"))
            }
            case default =>
          }
        })
        res = child + " " + assignmentOperator + " " + value
      }
      case "SuffixUnaryModificationExpression" => {
        var child : String = null
        var operator : String = null

        eObject.eClass().getEAllStructuralFeatures.forEach(structuralFeature => {
          structuralFeature.getName match {
            case "child" => {
              if(getObjectByStructuralFeatureName(eObject, "child").eCrossReferences().size() == 1) {
                val crossRefObject : EObject = getCrossReferenceObject(getObjectByStructuralFeatureName(eObject, "child"))
                child = crossRefObject.eGet(getAttributeByName(crossRefObject, "name")).toString
              }
            }
            case "operator" => {
              operator = getOperator(getObjectByStructuralFeatureName(eObject, "operator").eClass.getName)
            }
            case default =>
          }
        })
        res = child + " " + operator
      }
      case "RelationExpression" => {
        var childrens = new util.ArrayList[String]()
        var operator : String = null

        eObject.eContents().forEach(element => {
          element.eClass.getName match {
            case "IdentifierReference" => {
              if(element.eCrossReferences().size() == 1) {
                val crossRefObject : EObject = getCrossReferenceObject(element)
                childrens.add(crossRefObject.eGet(getAttributeByName(crossRefObject, "name")).toString)
              }
            }
            case "Addition" | "Multiplication" | "Division" | "Subtraction" | "GreaterThan" | "LessThan" => {
              operator = getOperator(element.eClass.getName)
            }
            case "DecimalIntegerLiteral" => {
              childrens.add(element.eGet(getAttributeByName(element, "decimalValue")).toString)
            }
          }
        })
        res = childrens.get(0) + " " + operator + " " + childrens.get(1)
      }
      case "EqualityExpression" => {
        var childrens = new util.ArrayList[String]()
        var operator : String = null

        eObject.eContents().forEach(element => {
          element.eClass.getName match {
            case "IdentifierReference" => {
              if(element.eCrossReferences().size() == 1) {
                val crossRefObject : EObject = getCrossReferenceObject(element)
                childrens.add(crossRefObject.eGet(getAttributeByName(crossRefObject, "name")).toString)
              }
            }
            case "Addition" | "Multiplication" | "Division" | "Subtraction" | "GreaterThan" | "LessThan" | "Equal" => {
              operator = getOperator(element.eClass.getName)
            }
            case "DecimalIntegerLiteral" => {
              childrens.add(element.eGet(getAttributeByName(element, "decimalValue")).toString)
            }
          }
        })
        res = childrens.get(0) + " " + operator + " " + childrens.get(1)
      }
      case "ExpressionStatement" => {
        var expression = getObjectByStructuralFeatureName(eObject, "expression")
        res = getValueOf(expression)
      }
    }
    res
  }

  def addObjectToContainer(simpleStmtEObject: EObject, container: EObject, containmentName: String) = {
    var res : EReference = null
    container.eClass().getEAllContainments().forEach(ref => {
      if(ref.getName.equals(containmentName)) res = ref
    })
    if(res == null) println("containmentName " + containmentName + " does not exist in the container " + container.eClass.getName)
    else{
      var list : util.List[EObject] = container.eGet(res).asInstanceOf[util.List[EObject]]
      list.add(simpleStmtEObject)
    }
  }

  def setObjectToContainer(simpleStmtEObject: EObject, container: EObject, containmentName: String) = {
    var res : EReference = null
    container.eClass().getEAllContainments().forEach(ref => {
      if(ref.getName.equals(containmentName)) res = ref
    })
    if(res == null) {
      println("containmentName " + containmentName + " does not exist in the container " + container.eClass.getName)
    }
    else{
      container.eSet(container.eClass.getEStructuralFeature(containmentName), simpleStmtEObject)
    }
  }

  private def compileLocalVariableStatement(ePackage: EPackage, flowGraphFactoryInstance: EFactory, node: EObject, parent: EObject) = {
    // Init SimpleStmt Object
    val simpleStmtEObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("SimpleStmt").asInstanceOf[EClass])
    val txtAttribute: EAttribute = getAttributeByName(simpleStmtEObject, "txt")

    // Getting fields from the XMI in input
    val variableObject = getObjectByStructuralFeatureName(node, "variable")
    val variableName = variableObject.eGet(getAttributeByName(variableObject, "name"))

    val typeReference : EObject  = getObjectByStructuralFeatureName(variableObject, "typeReference")
    val variableType = typeReference.eClass().getName

    val initialValueObject : EObject = getObjectByStructuralFeatureName(variableObject, "initialValue")

    var variableInitialValue : String = getValueOf(initialValueObject)

    simpleStmtEObject.eSet(txtAttribute, variableType + " " + variableName + " = " + variableInitialValue)

    if(parent != null)
      addObjectToContainer(simpleStmtEObject, parent, "stmts")
    simpleStmtEObject
  }

  private def compileExpressionStatement(ePackage: EPackage, flowGraphFactoryInstance: EFactory, node: EObject, parent: EObject) = {
    // Init SimpleStmt Object
    val simpleStmtEObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("SimpleStmt").asInstanceOf[EClass])
    val txtAttribute: EAttribute = getAttributeByName(simpleStmtEObject, "txt")
    // Getting fields from the XMI in input
    val expressionObject : EObject = getObjectByStructuralFeatureName(node, "expression")
    var expressionString : String = getValueOf(expressionObject)
    simpleStmtEObject.eSet(txtAttribute, expressionString)

    if(parent != null)
      addObjectToContainer(simpleStmtEObject, parent, "stmts")

    simpleStmtEObject
  }

  private def compileReturn(ePackage: EPackage, flowGraphFactoryInstance: EFactory, node: EObject, parent : EObject) = {
    // Init SimpleStmt Object
    val returnEObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("Return").asInstanceOf[EClass])
    val txtAttribute: EAttribute = getAttributeByName(returnEObject, "txt")

    // Variables we need
    var returnValue : Any = null

    // Getting fields from the XMI in input
    val returnObject : EObject = getObjectByStructuralFeatureName(node, "returnValue")
    if(returnObject != null)
      returnValue = getValueOf(returnObject)
    returnEObject.eSet(txtAttribute, "return " + returnValue + ";")

    if(parent != null)
      addObjectToContainer(returnEObject, parent, "stmts")
    returnEObject
  }

  private def compileBreak(ePackage: EPackage, flowGraphFactoryInstance: EFactory, node: EObject, parent : EObject) = {
    // Init SimpleStmt Object
    val breakEObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("Break").asInstanceOf[EClass])
    val txtAttribute: EAttribute = getAttributeByName(breakEObject, "txt")

    breakEObject.eSet(txtAttribute, "break;")

    if(parent != null)
      addObjectToContainer(breakEObject, parent, "stmts")
    breakEObject
  }

  def compileCondition(ePackage: EPackage, flowGraphFactoryInstance: EFactory, node: EObject, parent: EObject) : EObject = {
    val ifObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("If").asInstanceOf[EClass])
    ifObject.eSet(getAttributeByName(ifObject, "txt"), "if")

    var statements = node.eContents()
    var statementEObject = getObjectByStructuralFeatureName(node, "statement")
    if(statementEObject != null )
      statementEObject.eClass().getName match {
        case "Block" => {
          val eObject: EObject = compileBlock(ePackage, flowGraphFactoryInstance, statementEObject, ifObject)
        }
      }
    var conditionEObject = getObjectByStructuralFeatureName(node, "condition")
    if(conditionEObject  != null)
      conditionEObject.eClass().getName match {
        case "RelationExpression" | "EqualityExpression" => {
          val exprString : String = getValueOf(conditionEObject)
          val exprObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("Expr").asInstanceOf[EClass])
          exprObject.eSet(getAttributeByName(exprObject, "txt"), exprString)
          setObjectToContainer(exprObject, ifObject, "expr")
        }
      }
    var elseStatementEObject = getObjectByStructuralFeatureName(node, "elseStatement")
    if(elseStatementEObject != null)
      elseStatementEObject.eClass().getName match {
        case "ExpressionStatement" => {
          val relationExpressionString = getValueOf(getObjectByStructuralFeatureName(elseStatementEObject, "expression"))
        }
        case "Condition" => {
          compileCondition(ePackage, flowGraphFactoryInstance, elseStatementEObject, ifObject)
        }
      }

    if(parent != null){
      parent.eClass().getName match {
        case "Block" => addObjectToContainer(ifObject, parent, "stmts")
        case "If" => setObjectToContainer(ifObject, parent, "else")
      }
    }


    ifObject
  }

  def compileBlock(ePackage: EPackage, flowGraphFactoryInstance: EFactory, node: EObject, parent: EObject) : EObject = {
    val blockObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("Block").asInstanceOf[EClass])
    blockObject.eSet(getAttributeByName(blockObject, "txt"), "\\{...\\}")

    var statements = getListObjectsByStructuralFeatureName(node, "statements")
    statements.forEach(statement => {
      statement.eClass().getName match {
        case "Condition" => compileCondition(ePackage, flowGraphFactoryInstance, statement, blockObject)
        case "ExpressionStatement" => {
          var expressionString = getValueOf(statement)
          val expressionObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("SimpleStmt").asInstanceOf[EClass])
          val txtAttribute: EAttribute = getAttributeByName(expressionObject, "txt")
          expressionObject.eSet(txtAttribute, expressionString)
          addObjectToContainer(expressionObject, blockObject, "stmts")
        }
        case "Return" => {
          compileReturn(ePackage, flowGraphFactoryInstance, statement, blockObject)
        }
        case "Break" => {
          compileBreak(ePackage, flowGraphFactoryInstance, statement, blockObject)
        }

      }
    })

    if(parent != null){
      parent.eClass().getName match {
        case "Loop" => setObjectToContainer(blockObject, parent, "body")
        case "If" => {
          setObjectToContainer(blockObject, parent, "then")
        }
      }
    }

    blockObject
  }

  def compileStatement(ePackage: EPackage, flowGraphFactoryInstance: EFactory, node: EObject, parent: EObject) = {

    node.eClass().getName match {
      case "Block" => compileBlock(ePackage, flowGraphFactoryInstance, node, parent)
    }
  }

  private def compileWhileLoop(ePackage: EPackage, flowGraphFactoryInstance: EFactory, node: EObject, parent : EObject) = {
    // Init While Loop Object
    val whileLoopObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("Loop").asInstanceOf[EClass])
    val txtAttribute: EAttribute = getAttributeByName(whileLoopObject, "txt")
    whileLoopObject.eSet(txtAttribute, "while")

    val statementObject : EObject = getObjectByStructuralFeatureName(node, "statement")
    compileStatement(ePackage, flowGraphFactoryInstance, statementObject, whileLoopObject)

    val conditionObject : EObject = getObjectByStructuralFeatureName(node, "condition")
    val exprString : String = getValueOf(conditionObject)
    val exprObject = flowGraphFactoryInstance.create(ePackage.getEClassifier("Expr").asInstanceOf[EClass])
    exprObject.eSet(getAttributeByName(exprObject, "txt"), exprString)
    setObjectToContainer(exprObject, whileLoopObject, "expr")

    if(parent != null)
      addObjectToContainer(whileLoopObject, parent, "stmts")

    whileLoopObject
  }

  def displayTreeIterator(tree : TreeIterator[EObject]): Unit = {
    while (tree.hasNext()) {
      println(tree.next())
    }
  }

  def displayList(list : EList[_]): Unit = {
    list.forEach(elem => println(elem))
  }

}
