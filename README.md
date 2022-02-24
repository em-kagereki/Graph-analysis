
# Knowledge management: Knowledge Graph


The objective of this project is to 
-  Create a Neo4j graph database for a subset of [Hetionet](https://het.io/#:~:text=Hetionet%20is%20an%20integrative%20network,2%2C250%2C197%20relationships%20(24%20types).)  data centered on bile tract cancer.
-  Conduct a thorough analysis of the knowledge graph  using graph algorithms, such as centrality and link prediction, and 
- Interpret results the results of the analysis and identify interesting associations between previously unlinked nodes.


[![Metagraph](graph_schema.png)]

## Table of content

- [Installation](#installation)
    - [R and packages](#R and packages)
    - [Neo4j](#Neo4j)
- [Analysis workflow](#Analysis workflow)
    - [Extension](#extension)
    - [Database](#database)
- [Page setup](#page-setup)
    - [Upload the page tree file](#upload-the-page-tree-file)
    - [Go to the import view](#go-to-the-import-view)
    - [Import the page tree](#import-the-page-tree)
    - [SEO-friendly URLs](#seo-friendly-urls)
- [License](#license)
- [Links](#links)

## Installation

This document is for the latest Aimeos TYPO3 **21.10 release and later**.

### R and packages

The R version 4.1.2 (2021-11-01) -- "Bird Hippie" was used for the project. The packages used were:


- [`neo4r`](https://cran.r-project.org/web/packages/neo4r/index.html): Neo4J Driver, allowing you to query data on  Neo4J server and handle the results in R.
- [`tidyverse`](https://www.tidyverse.org/):Packages for data science.
-[`ggplot2](https://ggplot2.tidyverse.org/):  A system for declaratively creating graphics
- [`ggthemes`](https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/): ggplot2 extension.
- [`PupillometryR`](https://cran.r-project.org/web/packages/PupillometryR/index.html)
- `cowplot`
- `ggdist`


### Neo4j

**Note:** Neo4J version 4.4.3




### Report writing



## Codes

Setup TYPO3 as normal by creating a `FIRST_INSTALL` file in the `./public` directory:

```bash
touch public/FIRST_INSTALL
```

Open the URL of your installation in the browser and follow the steps in the TYPO3 setup scripts.


```
'DB' => [
    'Connections' => [
        'Default' => [
            'tableoptions' => [
                'charset' => 'utf8',
                'collate' => 'utf8_unicode_ci',
            ],
            // ...
        ],
    ],
],
```



## Site setup

TYPO3 10+ requires a site configuration which you have to add in "Site Management" > "Sites" available in the left navigation.

## Page setup

The page setup for an Aimeos web shop is easy if you import the example page tree for TYPO3 10/11:

* [21.10+ page tree](https://aimeos.org/fileadmin/download/Aimeos-pages_21.10.t3d) only

**Note:** The Aimeos layout expects [Bootstrap](https://getbootstrap.com) providing the grid layout!

### Go to the import view

* In Web::Page, root page (the one with the globe)
* Right click on the globe
* Move the cursor to "Branch actions"
* In the sub-menu, click on "Import from .t3d"

![Go to the import view](https://aimeos.org/docs/images/Aimeos-typo3-pages-menu.png)

### Upload the page tree file

* In the page import dialog
* Select the "Upload" tab (2nd one)
* Click on the "Select" dialog
* Choose the file you've downloaded
* Press the "Upload files" button

![Upload the page tree file](https://aimeos.org/docs/images/Aimeos-typo3-pages-upload.png)

### Import the page tree

* In Import / Export view
* Select the uploaded file from the drop-down menu
* Click on the "Preview" button
* The pages that will be imported are shown below
* Click on the "Import" button that has appeared
* Confirm to import the pages

![Import the uploaded page tree file](https://aimeos.org/docs/images/Aimeos-typo3-pages-import.png)

Now you have a new page "Shop" in your page tree including all required sub-pages.

### SEO-friendly URLs

TYPO3 9.5 and later can create SEO friendly URLs if you add the rules to the site config:
[https://aimeos.org/docs/latest/typo3/setup/#seo-urls](https://aimeos.org/docs/latest/typo3/setup/#seo-urls)

## License

The Aimeos TYPO3 extension is licensed under the terms of the GPL Open Source
license and is available for free.

## Links

* [Web site](https://aimeos.org/integrations/typo3-shop-extension/)
* [Documentation](https://aimeos.org/docs/TYPO3)
* [Forum](https://aimeos.org/help/typo3-extension-f16/)
* [Issue tracker](https://github.com/aimeos/aimeos-typo3/issues)
* [Source code](https://github.com/aimeos/aimeos-typo3)
