Sources that are used to build https://github.com/quasilyte/quasilyte.github.io.  
The site itself can be found here: https://quasilyte.dev/blog/.

Some information:
- [Hugo](https://github.com/gohugoio/hugo) static site generator
- [Steam](https://themes.gohugo.io/steam/) huge theme

`./hugo_hints.txt` - memo for `hugo` commands.

`./scripts/run_server` - run server on `127.0.0.1:1313`.

`./scripts/deploy` - build script.

Install steps:
```bash
# 1. Install hugo.

# 2. Install theme.
mkdir -p themes
cd themes
git clone https://github.com/digitalcraftsman/hugo-steam-theme.git

# 3. Check installation.
./script/run_server
```
