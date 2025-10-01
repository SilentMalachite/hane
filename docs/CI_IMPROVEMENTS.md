# CI Configuration Improvements

## Changes Applied

### 1. GHC Version Coverage ✅
**Issue**: CI only tested GHC 9.6.5, 9.6.7, 9.12.2 but `hane.cabal` claimed support for 9.8.2 and 9.10.1

**Fix**: Added GHC 9.8.2 and 9.10.1 to the CI matrix

```yaml
matrix:
  ghc: ["9.6.5", "9.6.7", "9.8.2", "9.10.1", "9.12.2"]
```

Now CI tests all versions listed in `tested-with` field.

### 2. Strict Warning Coverage ✅
**Issue**: Only library built with `-Werror`, executables and tests could have warnings

**Fix**: Changed to build all components with strict warnings

```yaml
- name: Build with strict warnings (-Wall -Werror, -f -icu)
  run: cabal build -f -icu all -j --ghc-options="-Wall -Werror"
```

This catches warnings in all code, not just the library.

### 3. Improved Cache Strategy ✅
**Issue**: Cache key didn't include cabal version, could cause cache invalidation issues

**Fix**: Added cabal version to cache key

```yaml
key: ${{ runner.os }}-ghc-${{ matrix.ghc }}-cabal-${{ matrix.cabal }}-${{ hashFiles('**/*.cabal', 'cabal.project*') }}
```

### 4. macOS ICU Support ✅
**Issue**: ICU only tested on Linux, macOS builds with ICU untested

**Fix**: Added macOS ICU installation and testing

```yaml
- name: Install ICU (macOS)
  if: runner.os == 'macOS'
  run: |
    brew install icu4c
    if [ -d "/opt/homebrew/opt/icu4c" ]; then
      echo "PKG_CONFIG_PATH=/opt/homebrew/opt/icu4c/lib/pkgconfig" >> $GITHUB_ENV
    else
      echo "PKG_CONFIG_PATH=/usr/local/opt/icu4c/lib/pkgconfig" >> $GITHUB_ENV
    fi
```

Handles both Apple Silicon (arm64) and Intel (x86_64) Macs.

### 5. Better Test Output ✅
**Fix**: Added `--test-show-details=streaming` for better test visibility

```yaml
- name: Test (-f -icu)
  run: cabal test -f -icu all -j --test-show-details=streaming
```

## Remaining Limitations

### Windows ICU
**Status**: Not tested in CI

**Reason**: Windows ICU setup is complex (requires vcpkg or manual DLL management)

**Mitigation**: 
- Documented in `docs/KNOWN_ISSUES.md`
- Default builds use `-f -icu` (ICU disabled)
- Users who need ICU on Windows can build manually

## CI Matrix Summary

| GHC Version | Ubuntu | macOS | Windows | ICU |
|-------------|--------|-------|---------|-----|
| 9.6.5       | ✅     | -     | -       | ✅  |
| 9.6.7       | ✅     | -     | -       | ✅  |
| 9.8.2       | ✅     | -     | -       | ✅  |
| 9.10.1      | ✅     | -     | -       | ✅  |
| 9.12.2      | ✅     | ✅    | ✅      | ✅* |

*ICU tested on Ubuntu and macOS only

## Build Time Optimization

- **Caching**: Cabal store and build artifacts cached per OS/GHC/Cabal combination
- **Parallel builds**: `-j` flag for parallel compilation
- **Dependency-only builds**: Separate step to cache dependencies
- **Fail-fast disabled**: All jobs complete even if one fails (better visibility)

## Quality Gates

1. **Format check** (fourmolu) - continues on error for signal only
2. **Lint check** (hlint) - continues on error for signal only  
3. **Strict build** (-Wall -Werror) - fails on any warning
4. **Tests** - must pass for all GHC versions
5. **ICU builds** - continues on error (platform-dependent)

## Future Improvements

- [ ] Consider Windows ICU testing with vcpkg
- [ ] Add benchmark tracking
- [ ] Add coverage reporting
- [ ] Pin action versions for reproducibility
- [ ] Add dependency freeze file check
