# Sourced by the pharos Cargo.lock checks.

# Print "name version origin" for each [[package]] in Cargo.lock $1. origin is
# "pharos#<sha>" for crates from the pharos git repo, "path" for path-sourced
# crates, and the raw source otherwise.
pharos_lock_packages() {
  awk '
    function flush() {
      if (name == "") return
      if (source == "") origin = "path"
      else if (tolower(source) ~ /^git\+https:\/\/github\.com\/a2-ai\/pharos(\.git)?[?#]/) { origin = source; sub(/^.*#/, "pharos#", origin) }
      else origin = source
      print name, version, origin
      name = version = source = ""
    }
    /^\[\[package\]\]/ { flush(); next }
    /^name = "/    { name = $3;    gsub(/"/, "", name) }
    /^version = "/ { version = $3; gsub(/"/, "", version) }
    /^source = "/  { source = $3;  gsub(/"/, "", source) }
    END { flush() }
  ' "$1"
}
