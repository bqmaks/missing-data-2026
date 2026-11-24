-- =============================================================================
--  Material — общий фильтр для форматов material-typst и material-revealjs.
--
--  Revealjs почти всё умеет сам, поэтому здесь для него только блоки
--  `::: {.block title="..."}`. Для typst фильтр добирает то, чего нет
--  в pandoc-писателе: колонки, классы слайдов, паузы и заметки докладчика.
-- =============================================================================

local is_typst = quarto.doc.is_format("typst")
local is_reveal = quarto.doc.is_format("revealjs")

-- Классы слайда, которые меняют вёрстку тела слайда (только typst).
local LAYOUT_CLASS = {
  center = true,
  smaller = true,
  larger = true,
}

-- Слайды без рамки собираются отдельной функцией темы.
local BARE_CLASS = {
  plain = true,
  inverse = true,
}

local function has(list, name)
  for _, v in ipairs(list) do
    if v == name then return true end
  end
  return false
end

-- Разбирает значение атрибута title как markdown и отдаёт inline-элементы.
local function title_inlines(title)
  local ok, parsed = pcall(pandoc.read, title, "markdown")
  if ok and parsed.blocks[1] and parsed.blocks[1].content then
    return parsed.blocks[1].content
  end
  return { pandoc.Str(title) }
end

-- Блок целиком в raw-typst: содержимое остаётся обычными блоками pandoc,
-- обрамляющие вызовы функций выводятся как raw. Писатель typst сохраняет порядок.
local function wrap_typst(open, content, close)
  local out = pandoc.List({ pandoc.RawBlock("typst", open) })
  out:extend(content)
  out:insert(pandoc.RawBlock("typst", close))
  return out
end

-- ---------------------------------------------------------------- Div ------
local function Div(el)
  local classes = el.classes

  -- ::: {.block title="Заголовок"} — блок в стиле beamer/tcolorbox
  if has(classes, "block") then
    local title = el.attributes["title"]
    if is_typst then
      local open = "#mat-block("
      if title then
        open = open .. "title: [" .. title .. "], "
      end
      return wrap_typst(open .. ")[", el.content, "]")
    elseif title then
      local head = pandoc.Div(pandoc.Plain(title_inlines(title)), pandoc.Attr("", { "block-title" }))
      local body = pandoc.Div(el.content, pandoc.Attr("", { "block-body" }))
      return pandoc.Div({ head, body }, el.attr)
    end
    return nil
  end

  if not is_typst then return nil end

  -- ::: {.columns} / ::: {.column width="40%"}
  if has(classes, "columns") then
    local out = pandoc.List({ pandoc.RawBlock("typst", "#mat-columns(") })
    for _, col in ipairs(el.content) do
      if col.t == "Div" and has(col.classes, "column") then
        local width = col.attributes["width"]
        local head = "("
        if width then head = head .. "width: " .. width .. ", " end
        out:insert(pandoc.RawBlock("typst", head .. "body: ["))
        out:extend(col.content)
        out:insert(pandoc.RawBlock("typst", "]),"))
      end
    end
    out:insert(pandoc.RawBlock("typst", ")"))
    return out
  end

  -- заметки докладчика в PDF не нужны
  if has(classes, "notes") or has(classes, "speaker-notes") then
    return {}
  end

  -- пошаговый вывод в PDF не поддерживается — просто разворачиваем
  if has(classes, "incremental") or has(classes, "nonincremental")
      or has(classes, "fragment") then
    return el.content
  end

  return nil
end

-- --------------------------------------------------------------- Para ------
-- `. . .` — пауза revealjs; в PDF её нужно убрать.
local function Para(el)
  if is_typst and pandoc.utils.stringify(el) == ". . ." then
    return {}
  end
  return nil
end

-- --------------------------------------------------------------- Meta ------
-- Короткие название и автор нужны рамке слайда в HTML: отдаём их скриптом.
local function Meta(meta)
  if not is_reveal then return nil end

  local function text_of(value)
    if value == nil then return nil end
    if type(value) == "table" and value.t == "MetaList" then
      local parts = {}
      for _, item in ipairs(value) do
        parts[#parts + 1] = pandoc.utils.stringify(item)
      end
      return table.concat(parts, ", ")
    end
    return pandoc.utils.stringify(value)
  end

  local function js_str(s)
    return '"' .. tostring(s or "")
      :gsub("\\", "\\\\"):gsub('"', '\\"'):gsub("<", "\\u003c"):gsub("\n", " ") .. '"'
  end

  local short_title = text_of(meta["short-title"]) or text_of(meta.title) or ""
  local short_author = text_of(meta["short-author"]) or text_of(meta.author) or ""

  quarto.doc.include_text("in-header", table.concat({
    "<script>window.matTheme = {shortTitle: ", js_str(short_title),
    ", shortAuthor: ", js_str(short_author), "};</script>",
  }))
  return nil
end

-- ------------------------------------------------------------ Pandoc -------
-- Тело слайда в typst нельзя обработать поэлементно: заголовок и его содержимое
-- лежат плоским списком. Поэтому классы слайда разбираются здесь, а тело
-- при необходимости заворачивается в #mat-vbox / #mat-plain-slide.
local function Pandoc(doc)
  if not is_typst then return nil end

  local blocks = pandoc.List({})
  local src = doc.blocks
  local i, n = 1, #src

  -- содержимое слайда: всё до следующего заголовка уровня 1–2
  local function slide_body(from)
    local body, j = pandoc.List({}), from
    while j <= n and not (src[j].t == "Header" and src[j].level <= 2) do
      body:insert(src[j])
      j = j + 1
    end
    return body, j
  end

  while i <= n do
    local b = src[i]

    local bare, layout = false, false
    if b.t == "Header" and b.level == 2 then
      for _, c in ipairs(b.classes) do
        if BARE_CLASS[c] then bare = true end
        if LAYOUT_CLASS[c] then layout = true end
      end
    end

    if bare then
      local body, j = slide_body(i + 1)
      local inverse = has(b.classes, "inverse")
      blocks:insert(pandoc.RawBlock("typst", "#mat-plain-slide(title: ["))
      blocks:insert(pandoc.Plain(b.content))
      blocks:insert(pandoc.RawBlock("typst",
        "], inverse: " .. tostring(inverse) .. ")["))
      blocks:extend(body)
      blocks:insert(pandoc.RawBlock("typst", "]"))
      i = j
    elseif layout then
      local body, j = slide_body(i + 1)
      local size = "1em"
      if has(b.classes, "smaller") then size = "0.82em" end
      if has(b.classes, "larger") then size = "1.18em" end
      blocks:insert(b)
      blocks:extend(wrap_typst(
        "#mat-vbox(vcenter: " .. tostring(has(b.classes, "center"))
          .. ", size: " .. size .. ")[",
        body, "]"))
      i = j
    else
      blocks:insert(b)
      i = i + 1
    end
  end

  doc.blocks = blocks
  return doc
end

return {
  { Meta = Meta },
  { Div = Div, Para = Para },
  { Pandoc = Pandoc },
}
