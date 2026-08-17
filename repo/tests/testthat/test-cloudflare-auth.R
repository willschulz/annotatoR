test_that("Cloudflare identity maps parse and normalize exact emails", {
  specification <- paste(
    "ghbragaw@syr.edu=ghbragaw@github",
    "msshumat@syr.edu=maddie-shumate@github",
    "wm.s.schulz@gmail.com=willschulz@github",
    sep = ","
  )

  mapping <- parse_cloudflare_identity_map(specification)
  expect_identical(
    unname(mapping),
    c("ghbragaw@github", "maddie-shumate@github", "willschulz@github")
  )
  expect_identical(
    resolve_cloudflare_identity(" GHBraGaw@SYR.EDU ", specification),
    "ghbragaw@github"
  )
  expect_identical(
    resolve_cloudflare_identity("msshumat@syr.edu", specification),
    "maddie-shumate@github"
  )
  expect_identical(
    resolve_cloudflare_identity("wm.s.schulz@gmail.com", specification),
    "willschulz@github"
  )
})

test_that("Cloudflare identity resolution fails closed", {
  specification <- "ghbragaw@syr.edu=ghbragaw@github"

  expect_null(resolve_cloudflare_identity(NULL, specification))
  expect_null(resolve_cloudflare_identity("", specification))
  expect_null(resolve_cloudflare_identity("unknown@syr.edu", specification))
  expect_error(
    parse_cloudflare_identity_map(""),
    "non-empty scalar"
  )
  expect_error(
    parse_cloudflare_identity_map("not-an-email=ghbragaw@github"),
    "valid email"
  )
  expect_error(
    parse_cloudflare_identity_map(
      "ghbragaw@syr.edu=one,ghbragaw@syr.edu=two"
    ),
    "duplicate"
  )
  expect_error(
    parse_cloudflare_identity_map("ghbragaw@syr.edu=one=two"),
    "exactly one"
  )
})
