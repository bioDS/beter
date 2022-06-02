# beter

Create [BEAST2](https://www.beast2.org/) XML files from individual XML chunks using [{{ moustache }}](https://mustache.github.io/) templating system and [TOML](https://github.com/toml-lang/toml) configuration files.

`beter` allows you to:
* write XML templates with {{ mustache }} tags
* {{ mustache }} tags are replaced by values in TOML config
* structure your TOML config with additional subconfigs

See [https://biods.github.io/beter/](https://biods.github.io/beter/) for more information.

## Installation
```r
devtools::install_github("biods/beter")
```

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
process_template("template.xml", "result.xml", "config.toml")
```
the `{{chain_length}}` will get replaced by `5000`. Instead of replacing a number, you can insert whole
XML chunks. This way you can programatically create templates with values and models you require.
