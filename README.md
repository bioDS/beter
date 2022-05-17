# beteR

Create [BEAST2](https://www.beast2.org/) XML files from individual XML chunks using [{{ moustache }}](https://mustache.github.io/) templating system and [TOML](https://github.com/toml-lang/toml) configuration files.

## Installation
```r
devtools::install_github("biods/beter")
```

## Rationale
Imagine that you are a scientists and you want to run analysis in BEAST2. So you create the input
XML file in BEAUTi GUI tool. However, not all BEAST2 packages, especially the cutting edge ones,
have BEAUTi templates. And even if they do, once you need to run the same analysis with different
parameters, change some submodel or run it with the new sequences you just got, you need to do the
same clicking again and again or manually edit the cumbersome XMLs.

This is especially true if you are developing new BEAST2 package and there is no BEAUTi template,
but you need to test your package throroughly under different parameter combinations.

Our `beter` package tries to solve this problem. Unlike other existing packages, it does not provide
you with function for pre-determined type of analyses and it does not directly depends on BEAST2
either. The `beter` package provides you with tools to make XML templates and subtemplates, which
you can parametrize with {{ mustache }} tags in a clean TOML config.

In short, the `beter` package allows you to:
* write XML templates with {{ mustache }} tags
* {{ mustache }} tags are replaced by values in TOML config
* structure your TOML config with additional subconfigs

## Usage
Lets have a (simplistic) BEAST2 XML with all the required parts. What we often need to do is run the same file for different number of generations because it often turns out that our first run didn't have enough ESS. This can be easily done by replacing specific number for `chainLength` with a {{ mustache }} tag:
```
<!--template.xml-->
<xml>
...
<run id="mcmc" spec="MCMC" chainLength="{{chain_length}}">
...
</xml>
```
and writing a TOML config for this:
```
# config.toml
[defaults]
chain_length = 5000
```
Now by running:
```r
library(beter)
process_template("template.xml", "config.toml", "result.xml")
```
the `{{chain_length}}` will get replaced by `5000`. Instead of replacing a number, you can insert whole
XML chunks. This way you can programatically create templates with values and models you require.
