# Un client Constellation.

Cette classe se connecte à un serveur Constellation déjà ouvert. Nous
vous recommandons de ne pas l'utiliser directement, mais plutôt
d'appeler \`constellationR::avecClientEtServeur\`, ou bien
\`constellationR::avecClient\`, lesquels s'occuperont de la création et
de la fermeture du client pour vous.

## Methods

### Public methods

- [`Client$new()`](#method-ClientConstellation-new)

- [`Client$action()`](#method-ClientConstellation-action)

- [`Client$suivre()`](#method-ClientConstellation-suivre)

- [`Client$rechercher()`](#method-ClientConstellation-rechercher)

- [`Client$appeler()`](#method-ClientConstellation-appeler)

- [`Client$enregistrerÉcoute()`](#method-ClientConstellation-enregistrer%C3%89coute)

- [`Client$obtDonnéesTableau()`](#method-ClientConstellation-obtDonn%C3%A9esTableau)

- [`Client$obtDonnéesTableauNuée()`](#method-ClientConstellation-obtDonn%C3%A9esTableauNu%C3%A9e)

- [`Client$fermer()`](#method-ClientConstellation-fermer)

- [`Client$clone()`](#method-ClientConstellation-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    Client$new(port, codeSecret = NULL)

#### Arguments

- `port`:

  Le numéro du port local sur lequel le serveur est ouvert, et auquel le
  client se connectera.

- `codeSecret`:

  Le code secret pour pouvoir se connecter au serveur.

  Fonction pour invoquer un action sur Constellation.

------------------------------------------------------------------------

### Method `action()`

#### Usage

    Client$action(fonction, paramètres = NULL, patience = 15)

#### Arguments

- `fonction`:

  Le nom de la fonction à invoquer (p. ex., "variables.créerVariable")

- `paramètres`:

  Liste nommée avec les paramètres de la fonction

- `patience`:

  Le nombre de secondes qu'on va attendre pour une réponse avant de
  perdre patience.

#### Returns

Le résultat ded la fonction invoquée.

Fonction pour invoquer un suivi sur Constellation.

------------------------------------------------------------------------

### Method `suivre()`

#### Usage

    Client$suivre(
      fonction,
      paramètres = NULL,
      nomArgFonction = "f",
      condition = function(x) !is.null(x),
      patience = 15
    )

#### Arguments

- `fonction`:

  Le nom de la fonction à invoquer (p. ex., "profil.suivreNoms")

- `paramètres`:

  Liste nommée avec les paramètres de la fonction

- `nomArgFonction`:

  Le nom du paramètre correspondant à la fonction de suivi (voir
  documentation IPA Constellation). "f" par défaut.

- `condition`:

  Condition nécessaire pour valider le premier résultat à retourner.
  Uniquement utilisé si \`paramètres\[\[nomArgFonction\]\]\` n'existe
  pas.

- `patience`:

  Le nombre de secondes qu'on va attendre pour une réponse avant de
  perdre patience.

#### Returns

Si \`paramètres\[\[nomArgFonction\]\]\` existe, cette fonction sera
invoqué de manière continue chaque fois que les résultats changent, et
la fonction \`suivre\` elle-même retournera une fonction pour annuler le
suivi. Si \`paramètres\[\[nomArgFonction\]\]\` n'existe pas, retourne le
premier résultat obtenu.

Fonction pour invoquer une recherche sur Constellation.

------------------------------------------------------------------------

### Method `rechercher()`

#### Usage

    Client$rechercher(fonction, paramètres, nomArgFonction = "f", patience = 15)

#### Arguments

- `fonction`:

  Le nom de la fonction à invoquer (p. ex.,
  "recherche.rechercherVariablesSelonNom")

- `paramètres`:

  Liste nommée avec les paramètres de la fonction

- `nomArgFonction`:

  Le nom du paramètre correspondant à la fonction de suivi (voir
  documentation IPA Constellation). "f" par défaut.

- `patience`:

  Le nombre de secondes qu'on va attendre pour une réponse avant de
  perdre patience.

#### Returns

Si \`paramètres\[\[nomArgFonction\]\]\` existe, cette fonction sera
invoqué de manière continue chaque fois que les résultats de la
recherche changent, et la fonction \`recherche\` elle-même retournera
des fonctions pour annuler la recherche et pour changer le nombre de
résultats désirés. Si \`paramètres\[\[nomArgFonction\]\]\` n'existe pas,
retourne le premier résultat obtenu par la recherche.

Fonction pour invoquer une fonction (action, recherche, ou suivi) de
Constellation.

------------------------------------------------------------------------

### Method `appeler()`

#### Usage

    Client$appeler(
      fonction,
      paramètres = NULL,
      nomArgFonction = "f",
      patience = 45
    )

#### Arguments

- `fonction`:

  Le nom de la fonction à invoquer (p. ex., "bds.créerBd")

- `paramètres`:

  Liste nommée avec les paramètres de la fonction, si approprié

- `nomArgFonction`:

  S'il s'agit d'un fonction de suivi ou de recherche, le nom du
  paramètre correspondant à la fonction de suivi (voir documentation IPA
  Constellation). "f" par défaut.

- `patience`:

  Le nombre de secondes qu'on va attendre pour une réponse avant de
  perdre patience.

#### Returns

Le résultat de la fonction

Méthode privée. Touche pas.

------------------------------------------------------------------------

### Method `enregistrerÉcoute()`

#### Usage

    Client$enregistrerÉcoute(idRequête, résoudre, rejeter, f = NULL)

#### Arguments

- `idRequête`:

  Identifiant unique

- `résoudre`:

  Fonction résolution

- `rejeter`:

  Fonction rejet

- `f`:

  Fonction de suivi

  Fonction rapide pour obtenir des données d'un tableau en format tibble

------------------------------------------------------------------------

### Method `obtDonnéesTableau()`

#### Usage

    Client$obtDonnéesTableau(idTableau, langues = NULL)

#### Arguments

- `idTableau`:

  L'identifiant du tableau

- `langues`:

  Liste optionnelle des langues (en ordre de préférence) dans lesquelles
  on veut obtenir les résultats

#### Returns

Les données en format tibble::tibble

Fonction rapide pour obtenir des données d'une nuée en format tibble

------------------------------------------------------------------------

### Method `obtDonnéesTableauNuée()`

#### Usage

    Client$obtDonnéesTableauNuée(
      idNuée,
      clefTableau,
      nRésultatsDésirés = 100,
      langues = NULL
    )

#### Arguments

- `idNuée`:

  L'identifiant de la nuée

- `clefTableau`:

  La clef du tableau d'intérêt

- `nRésultatsDésirés`:

  Le nombre de résultats désirés

- `langues`:

  Liste optionnelle des langues (en ordre de préférence) dans lesquelles
  on veut obtenir les résultats

#### Returns

Les données en format tibble::tibble

Fermer le client

------------------------------------------------------------------------

### Method `fermer()`

#### Usage

    Client$fermer()

#### Returns

Rien

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Client$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
