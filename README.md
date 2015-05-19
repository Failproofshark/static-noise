# Static Noise

## License
[MIT License](http://opensource.org/licenses/MIT)

## Introduction

Static Noise is a simple blog-centric static site generator written in Common Lisp. It's really more of a static site command language than a simple generator.

## Usage

After loading the system set it as your main package. Once you have done that you could either create a new blog with the `create-blog` function and giving it a path specifier for the root directory of your blog, the title, a description of your blog (used for the generated RSS feed), and a url to your blog

```
(create-blog #p"/home/foo/myblog/" "blog title" "describe your blog" "www.myblog.com")
```

The directory will be created as well as a basic scaffolding for you to work with. If you already have a blog you wish to work with you may use the `open-blog` function giving it the absolute pathname where you keep your blog

```
(open-blog #p"/home/foo/myblog/")
```

When you it's time to render simple run the `render-blog` function with no arguments

```
(render-blog)
```

This will render all the blog posts, pages, an archive and copy all the files in the static folder into another folder called `rendered`.

I use [Hunchentoot](https://github.com/edicl/hunchentoot) to provide a simple dev server to allow you to preview your work prior to uploading. Just point your browser to localhost:8080 after starting the dev server

```
;; Starting the dev server
(start-dev-server)
;; Stopping the ev server
(stop-dev-server)
```

## Scaffolding

When you run the `create-blog` function it will bring up some scaffolding for your blog.

### Config

Configuration is pretty sparce at the moment, and is a simple plist that sets the templates to be used for rendering the different types of pages for your blog. Configuration must go into the file called `config.lisp` in the root directory. You only need to enter the name of the template since all templates are read from the `templates` directory. A default configuration is started for you upon creation.

```
(:blog-configuration :TITLE "Your blog name"
                     :description "A description"
                     :url "www.something.com"
                     :ARTICLE "article.html"
                     :ARCHIVE "archive.html"
                     :PAGE "page.html"
                     :RSS-TEMPLATE "feed-template.xml")
```

### Templates

Templates are compiled with [Djula](https://mmontone.github.io/djula/) and it's syntax is as such. All templates go into the `templates` directory. When configuring the templates in the `config.lisp` file you only need to list the final template page (no need to enter the layout it's based on, in other words)

### Content

All blog entries go into the `content` folder. Blog entries are written in [markdown](https://daringfireball.net/projects/markdown/). The very first line of every entry must be meta data containing a title and publish date in a "MM-DD-YYYY" format. An example of this is as follows

```
{metdata (:title "My First Post" :date-created "11-29-2014")}
```

### Pages

All static pages (about, blog roll, resume, etc.) goes in the directory called `pages`. Like the articles they are written in markdown. Unlike the articles the only metadata needed for pages are the title

```
{metdata (:title "About me")}
```

### Static

All css, javascript, and image assets go here. This folder will simply be copied over when rendering.

