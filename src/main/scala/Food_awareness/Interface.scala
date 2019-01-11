import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property._
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{ Insets, Pos }
import scalafx.scene.control._
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.shape._
import scalafx.scene.paint.{ Color, LinearGradient, RadialGradient }
import scalafx.scene.{ Scene, Group, Node }
import scalafx.event.ActionEvent
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.geometry.Orientation;
import scalafx.scene.Scene
import scalafx.scene.effect.DropShadow
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{ Stops, LinearGradient }
import scalafx.scene.text.Text
import scalafx.scene.text.TextAlignment
import scalafx.scene.image._
import scalafx.scene.control.ScrollBar
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import java.io.File
import scala.io.Source
import io.StdIn

class Produit(nom: String, genre: String, kcal: String, kj: String, lipides: String, acides: String, glucides: String, fibres: String, proteines: String, sel: String, provenance: String, indice: Int, url: String) {
  val n = nom
  val ge = genre
  val kc = kcal
  val k = kj
  val lip = lipides
  val acid = acides
  val gluc = glucides
  val fib = fibres
  val prot = proteines
  val s = sel
  val prov = provenance
  var const = ObservableBuffer(n, ge, kc, k, lip, acid, gluc, fib, prot, s, prov)
  var note = 0
  val ind = indice

  def getVal(): ObservableBuffer[String] = {
    return const
  }

  def getName(): String = {
    return n
  }

  def setNote(m: Int): Unit = {
    note = m
  }

  def getNote(): Int = {
    return note
  }

  def getInd(): Int = {
    return ind
  }

  def getURL(): String = {
    return url
  }
}

object TestFX extends JFXApp {

  var tempTab = 0 //indique quel produit est affiché sur la fenêtre 
  var rating = 10
  var click = false
  var switch = false
  var listeProduits = lireDonnees
  var tab = ObservableBuffer[Produit]() //tableau de produits
  for (x <- listeProduits) {
    tab += x
  }
  var texteAff = "";
  var commentaire = "";
  var tabBoxCommentaire = ArrayBuffer[ArrayBuffer[Rectangle]]()//tableaux à deux dimensions permettant de stocker et de créer les commentaires pour ch
  var tabUsername = ArrayBuffer[ArrayBuffer[Text]]()
  var tabCommentaire = ArrayBuffer[ArrayBuffer[Text]]()
  for (x <- 0 until listeProduits.length) {
    tabBoxCommentaire += ArrayBuffer[Rectangle]()
    tabUsername += ArrayBuffer[Text]()
    tabCommentaire += ArrayBuffer[Text]()
  }
  var nbCommentaires = Array.fill(listeProduits.length)(0) //indique nb de commentaires sur chaque produit
  //amélioration : créer classe commentaires et ajouter objet commentaire comme attribut de produit

  stage = new JFXApp.PrimaryStage {
    title = "Open Food"
    scene = new Scene(1200, 1000) {
      //Page d'accueil
      val texteProduit = new Text {
        text = "Open Food"
        style = "-fx-font-size: 48pt"
        fill = new LinearGradient(
          endX = 0,
          stops = Stops(PaleGreen, SeaGreen))
      }
      texteProduit.layoutY = 80
      texteProduit.layoutX = 450
      texteProduit.setTextAlignment(TextAlignment.CENTER)
      
      val button = new Button("Rechercher")
      button.layoutX = 650
      button.layoutY = 150
      
      val texte = new TextField
      texte.layoutX = 470
      texte.layoutY = 150
      
      //Création cercles pour note utilisateur
      var tabCerclesUtil = ArrayBuffer[Circle]()
      for (x <- 0 to 4) {
        tabCerclesUtil += Circle((590 + 22 * x), 550, 10)
        tabCerclesUtil(x).fill = Color.White
        tabCerclesUtil(x).stroke = Color.Black
      }
      
      //indice santé/pollution
      var tabCerclesIndice = ArrayBuffer[Circle]()
      for (x <- 0 to 4) {
        tabCerclesIndice += Circle((220 + 22 * x), 580, 10)
        tabCerclesIndice(x).fill = Color.White
        tabCerclesIndice(x).stroke = Color.Black
      }
      val indiceTexte = new Text {
        text = "Indice (Santé/Pollution) : "
        style = "-fx-font-size: 10pt"
      }
      indiceTexte.layoutX = 35
      indiceTexte.layoutY = 585
      
      //Zone commentaires
      val commenter = new TextArea
      commenter.layoutX = 800
      commenter.layoutY = 240
      commenter.setPrefSize(300, 70)
      val button1 = new Button("Commenter")
      button1.layoutX = 1107
      button1.layoutY = 240

      
      //liste des composants du produit
      val table = new ListView(ObservableBuffer(" "))
      table.layoutX = 210
      table.layoutY = 240
      table.setPrefHeight(280)
      
      //liste nom des composants
      val table1 = new ListView(ObservableBuffer("Nom", "Type", "Énergie en kcal", "Énergie en kJ", "Lipides", "Acides gras saturés", "Glucides", "Fibres alimentaires", "Protéines", "Sel", "Provenance"))
      table1.layoutX = 70
      table1.layoutY = 240
      table1.setPrefSize(140, 280)

      //Récupération des images des produits
      var tabImage = ArrayBuffer[Image]()
      for (x <- tab) {
        tabImage += new Image(x.getURL())
      }
      val view = new ImageView(tabImage(1))
      view.setScaleX(0.3)
      view.setScaleY(0.3)
      view.layoutX = 200
      view.layoutY = -50

      content = List(texte, button, texteProduit)

      table.getItems.clear()
      
      //Clique sur rechercher
      button.onAction = (e: ActionEvent) => {
        
        //Enlever les éléments affichés si présents
        if (content.contains(table)) {
          table.getItems.clear()
          switch = false
          //A améliorer : mettre dans un tableau ?
          content.remove(content.indexOf(table))
          content.remove(content.indexOf(table1))
          content.remove(content.indexOf(view))
          content.remove(content.indexOf(commenter))
          content.remove(content.indexOf(button1))
          content.remove(content.indexOf(indiceTexte))

          if (nbCommentaires(tempTab) > 0)
            for (x <- 0 until nbCommentaires(tempTab)) {
              content.remove(content.indexOf(tabBoxCommentaire(tempTab)(x)))
              content.remove(content.indexOf(tabUsername(tempTab)(x)))
              content.remove(content.indexOf(tabCommentaire(tempTab)(x)))
            }
          for (x <- 0 until tabCerclesUtil.length) {
            content.remove(content.indexOf(tabCerclesUtil(x)))
            content.remove(content.indexOf(tabCerclesIndice(x)))
          }
        }
        //Vérification si mot entré correspond à un produit
        //A améliorer
        texteAff = texte.text.apply()
        for (x <- 0 to tab.length - 1) {
          if (tab(x).getName().toLowerCase().contains(texteAff.toLowerCase())) {
            tempTab = x
            view.setImage(tabImage(x))
            for (y <- 0 until tab(x).getVal().length) {
              table.getItems.add(tab(x).getVal()(y))
            }
            switch = true;
          }
        }
        //Affichage infos produit
        if (switch) {
          content += commenter
          content += button1
          content += view
          content += table
          content += table1
          content += indiceTexte
          for (x <- 0 until tabCerclesUtil.length) {
            content += tabCerclesUtil(x)
            content += tabCerclesIndice(x)
          }
          for (y <- tab(tempTab).getInd() until tabCerclesUtil.length) {
            tabCerclesIndice(y).fill = Color.White
          }
          for (y <- 0 until tab(tempTab).getInd()) {
            tabCerclesIndice(y).fill = Color.Green
          }
          for (y <- tab(tempTab).getNote() + 1 until tabCerclesUtil.length) {
            tabCerclesUtil(y).fill = Color.White
          }
          for (y <- 0 until tab(tempTab).getNote() + 1) {
            tabCerclesUtil(y).fill = Color.GOLD
          }
          if(nbCommentaires(tempTab) > 0){
            for (x <- 0 until nbCommentaires(tempTab)){
              content += tabBoxCommentaire(tempTab)(x)
              content += tabUsername(tempTab)(x)
              content += tabCommentaire(tempTab)(x)
            }
          }
        }
      }
      
      //Ajout d'un commentaire et stockage dans tableau à deux dimensions
      button1.onAction = (e: ActionEvent) => {
        commentaire = commenter.text.apply()

        tabBoxCommentaire(tempTab) += Rectangle(300, 70)
        tabBoxCommentaire(tempTab)(nbCommentaires(tempTab)).layoutX = 800
        tabBoxCommentaire(tempTab)(nbCommentaires(tempTab)).layoutY = 320 + (80 * nbCommentaires(tempTab))
        tabBoxCommentaire(tempTab)(nbCommentaires(tempTab)).fill = Color.White
        tabBoxCommentaire(tempTab)(nbCommentaires(tempTab)).stroke = Color.Black
        tabUsername(tempTab) += new Text {
          text = "Utilisateur"
          style = "-fx-font-size: 10pt"
        }
        tabUsername(tempTab)(nbCommentaires(tempTab)).layoutX = 805
        tabUsername(tempTab)(nbCommentaires(tempTab)).layoutY = 334 + (80 * nbCommentaires(tempTab))
        tabCommentaire(tempTab) += new Text {
          text = commentaire
          style = "-fx-font-size: 8pt"
        }
        tabCommentaire(tempTab)(nbCommentaires(tempTab)).layoutX = 810
        tabCommentaire(tempTab)(nbCommentaires(tempTab)).layoutY = 355 + (80 * nbCommentaires(tempTab))

        content += tabBoxCommentaire(tempTab)(nbCommentaires(tempTab))
        content += tabUsername(tempTab)(nbCommentaires(tempTab))
        content += tabCommentaire(tempTab)(nbCommentaires(tempTab))
        nbCommentaires(tempTab) += 1
      }

      for (x <- 0 until tabCerclesUtil.length) {
        //Définir une note pour un produit
        tabCerclesUtil(x).onMouseClicked = (e: MouseEvent) => {
          tab(tempTab).setNote(x)
        }
        
        //Animation lors de survol de souris
        tabCerclesUtil(x).onMouseEntered = (e: MouseEvent) => {
          for (y <- 0 to x) {
            tabCerclesUtil(y).fill = Color.GOLD
          }
          for (z <- x + 1 until tabCerclesUtil.length) {
            tabCerclesUtil(z).fill = Color.White
          }
        }
        
        //Rétablir la note originale
        tabCerclesUtil(x).onMouseExited = (e: MouseEvent) => {
          if (tab(tempTab).getNote() != tabCerclesUtil.length && tab(tempTab).getNote() != 10) {
            for (y <- tab(tempTab).getNote() + 1 until tabCerclesUtil.length) {
              tabCerclesUtil(y).fill = Color.White
            }
            for (y <- 0 until tab(tempTab).getNote() + 1) {
              tabCerclesUtil(y).fill = Color.GOLD
            }
          } else if (tab(tempTab).getNote() == 10) {
            for (y <- 0 until tabCerclesUtil.length) {
              tabCerclesUtil(y).fill = Color.White
            }
          }
        }
      }
    }
  }
  stage.close()
  /**
   * Méthode lisant les données d'un fichier txt
   */
  def lireDonnees: ArrayBuffer[Produit] = {
    val fr = Source.fromFile("ressources/donnees_produits.txt")
    val ligneFr = fr.reset().getLines()
    var listeProduits = ArrayBuffer[Produit]()

    while (!ligneFr.isEmpty) {
      var ligne = ligneFr.next()
      var produit = ligne.split(",")
      listeProduits += new Produit(produit(0), produit(1), produit(2), produit(3), produit(4), produit(5), produit(6), produit(7), produit(8), produit(9), produit(10), produit(11).toInt, produit(12)) //Chaque ligne du fichier correspond à un porte-monnaie
    }
    return listeProduits
  }
}