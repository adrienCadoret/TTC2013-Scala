import java.io.IOException
import java.util

import org.eclipse.emf.common.util.{EList, TreeIterator, URI}
import org.eclipse.emf.ecore.{EAttribute, EFactory, EObject, EPackage, EReference}
import org.eclipse.emf.ecore.resource.{Resource, ResourceSet}
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import org.eclipse.emf.ecore.xmi.XMIResource
import org.eclipse.emf.ecore.xmi.impl.{EcoreResourceFactoryImpl, XMIResourceFactoryImpl}
import ControlFlowGenerator._
import org.eclipse.emf.ecore.util.EcoreUtil

object DataFlowGenerator extends App {
  val resourceSet : ResourceSet  = new ResourceSetImpl()

  createDataFlowGraphModelInstance(loadDataFlowGraphMetaModel(), "Test0-ControlFlowGraph.xmi" , "Test0-DataFlowGraph.xmi")

  def loadDataFlowGraphMetaModel(): Resource = {
    resourceSet.getResourceFactoryRegistry.getExtensionToFactoryMap.put("ecore", new EcoreResourceFactoryImpl())
    resourceSet.getResourceFactoryRegistry.getExtensionToFactoryMap.put("xmi", new XMIResourceFactoryImpl())

    resourceSet.getResource(URI.createFileURI("./FlowGraph.ecore"), true)
  }

  def createDataFlowGraphModelInstance(metaModel : Resource, uriSource: String, createdFile: String) : Any = {
    // Creating ressource
    val resource : Resource  = resourceSet.createResource(URI.createURI(createdFile))
    // Getting the right package ,  the only one
    val ePackage = metaModel.getContents.get(0).asInstanceOf[EPackage]
    // Creating a factory to generate an instance of the metamodel
    val flowGraphFactoryInstance : EFactory  = ePackage.getEFactoryInstance

    // Loop on the XMI in input to generate an instance of the model
    //generateModel(resource, resourceSet.getResource(URI.createURI(uriSource), true).getAllContents, ePackage, flowGraphFactoryInstance, null)
    generateModel(resource, resourceSet.getResource(URI.createURI(uriSource), true).getAllContents, ePackage, flowGraphFactoryInstance)

    // Options for the output file
    val options = new util.HashMap[String, Boolean]()
    options.put(XMIResource.XMI_URI, true)

    try
      resource.save(options)
    catch {
      case e: IOException => e.printStackTrace()
    }
  }

  def generateModel(resource: Resource, tree: TreeIterator[EObject], ePackage: EPackage, flowGraphFactoryInstance: EFactory) : Void = {
    val node = tree.next()

    val definitions = new util.HashMap[String, EObject]()
    val method: EObject = EcoreUtil.copy(tree.next())
    val treeCopy = method.eAllContents

    while (treeCopy.hasNext) {
      val node = treeCopy.next
      println(node.eClass().getName)

      val txt = node.eGet(getAttributeByName(node, "txt")).asInstanceOf[String]

      if (txt.split("=").length > 1) { // Cas simple d'une définition
        println("*** " + txt)

        // On découpe en deux parties pour savoir quand une variable a été utilisée
        val partieGauche = txt.split("=")(0).trim
        val partieDroite = txt.split("=")(1)

        // Permet de connaitre le nom de la variable définie
        var variableDefinie = ""
        if (partieGauche.split("\\s+").length <= 1) { // de la forme : a = ...
          variableDefinie = partieGauche
        } else { // de la forme : int a = ...
          variableDefinie = partieGauche.split("\\s+")(1)
        }



        // Pour chaque variable déjà définie, on regarde dans la partie droite du statement actuel si la variable est présente
        definitions.keySet().forEach(variable => {
          if (partieDroite.indexOf(variable) != -1) { // Dans ce cas elle a été utilisée, donc on met à jour son attribut dfNext
            definitions.get(variable).eGet(node.eClass().getEStructuralFeature("dfNext")).asInstanceOf[util.Collection[EObject]].add(node)
          }
        })


        // Dans tous les cas on met à jour le node ou la variable a été définie
        definitions.put(variableDefinie, node)
      } else if (txt.indexOf("return") != -1){ // return
        val valRetour = txt.split("return")(1).trim

        definitions.keySet().forEach(variable => {
          if (valRetour.indexOf(variable) != -1) { // Dans ce cas elle a été utilisée, donc on met à jour son attribut dfNext
            definitions.get(variable).eGet(node.eClass().getEStructuralFeature("dfNext")).asInstanceOf[util.Collection[EObject]].add(node)
          }
        })
      } else { // Boucles et autre

      }
    }
  resource.getContents.add(method)
//    compileMethod(resource, ePackage, flowGraphFactoryInstance, node)

    null
  }
}
