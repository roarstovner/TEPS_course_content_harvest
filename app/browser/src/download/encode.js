// File formats of a download: CSV, XLSX and Parquet, plus a CSV reader for
// the data dictionary. XLSX is a minimal writer of our own on top of fflate
// (inline strings, one sheet, a bold frozen header): the npm `xlsx` package is
// outdated and other writers bring more than a table needs. The libraries are
// imported when a file is made, so they stay out of the main bundle.

/** Excel's limit of characters in a cell. */
export const EXCEL_MAX = 32767

const text = (v) =>
  v == null ? '' : typeof v === 'boolean' ? (v ? 'TRUE' : 'FALSE') : v instanceof Date ? isoDate(v) : String(v)
const isoDate = (d) => d.toISOString().slice(0, 10)

/** CSV (RFC 4180): UTF-8 with BOM for Excel, CRLF between records, fields
 *  with a comma, quote or line break quoted; line breaks inside are kept. */
export function toCsv(cols, rows) {
  const field = (v) => {
    const s = text(v)
    return /[",\r\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s
  }
  const lines = [cols.map(field).join(',')]
  for (const r of rows) lines.push(cols.map((c) => field(r[c])).join(','))
  return `﻿${lines.join('\r\n')}\r\n`
}

/** Rows of a CSV text (RFC 4180, header in the first line) as objects. */
export function parseCsv(s) {
  s = s.replace(/^﻿/, '')
  const recs = []
  let rec = []
  let f = ''
  let quoted = false
  for (let i = 0; i < s.length; i++) {
    const ch = s[i]
    if (quoted) {
      if (ch === '"' && s[i + 1] === '"') (f += '"'), i++
      else if (ch === '"') quoted = false
      else f += ch
    } else if (ch === '"') quoted = true
    else if (ch === ',') rec.push(f), (f = '')
    else if (ch === '\n' || ch === '\r') {
      if (ch === '\r' && s[i + 1] === '\n') i++
      rec.push(f), recs.push(rec), (rec = []), (f = '')
    } else f += ch
  }
  if (f || rec.length) rec.push(f), recs.push(rec)
  const [head, ...body] = recs
  return body.map((r) => Object.fromEntries(head.map((h, j) => [h, r[j] ?? ''])))
}

/**
 * Cut text cells over Excel's limit and flag the row in a `truncated` column
 * (added when `flag` is true, whether or not a cell was cut).
 * @returns {{cols, rows, truncated: number}} truncated: rows cut
 */
export function truncateForExcel(cols, rows, flag) {
  let truncated = 0
  const out = rows.map((r) => {
    let cut = false
    const o = { ...r }
    for (const c of cols)
      if (typeof o[c] === 'string' && o[c].length > EXCEL_MAX) {
        let end = EXCEL_MAX
        if (/[\uD800-\uDBFF]/.test(o[c][end - 1])) end-- // keep surrogate pairs whole
        o[c] = o[c].slice(0, end)
        cut = true
      }
    if (cut) truncated++
    if (flag) o.truncated = cut
    return o
  })
  return { cols: flag ? [...cols, 'truncated'] : cols, rows: out, truncated }
}

// characters XML 1.0 does not allow
// eslint-disable-next-line no-control-regex
const BAD_XML = /[^\x09\x0A\x0D\x20-퟿-�\u{10000}-\u{10FFFF}]/gu
const esc = (s) => s.replace(BAD_XML, '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
const colName = (i) => (i < 26 ? '' : colName(Math.floor(i / 26) - 1)) + String.fromCharCode(65 + (i % 26))

const XML = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
const NS = 'http://schemas.openxmlformats.org'
const PARTS = {
  '[Content_Types].xml': `${XML}<Types xmlns="${NS}/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/><Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/></Types>`,
  '_rels/.rels': `${XML}<Relationships xmlns="${NS}/package/2006/relationships"><Relationship Id="rId1" Type="${NS}/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>`,
  'xl/_rels/workbook.xml.rels': `${XML}<Relationships xmlns="${NS}/package/2006/relationships"><Relationship Id="rId1" Type="${NS}/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/><Relationship Id="rId2" Type="${NS}/officeDocument/2006/relationships/styles" Target="styles.xml"/></Relationships>`,
  'xl/styles.xml': `${XML}<styleSheet xmlns="${NS}/spreadsheetml/2006/main"><fonts count="2"><font><sz val="11"/><name val="Calibri"/></font><font><b/><sz val="11"/><name val="Calibri"/></font></fonts><fills count="2"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill></fills><borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders><cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs><cellXfs count="2"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/><xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/></cellXfs></styleSheet>`,
}

/**
 * An XLSX workbook of one sheet. Cut cells first (`truncateForExcel`): Excel
 * refuses a file with a longer cell.
 * @returns {Promise<Uint8Array>}
 */
export async function toXlsx(cols, rows, sheet = 'data') {
  const { strToU8, zipSync } = await import('fflate')
  const cell = (ref, v, s = '') => {
    if (v == null || v === '') return ''
    if (typeof v === 'number' && Number.isFinite(v)) return `<c r="${ref}"${s}><v>${v}</v></c>`
    if (typeof v === 'boolean') return `<c r="${ref}" t="b"${s}><v>${v ? 1 : 0}</v></c>`
    return `<c r="${ref}" t="inlineStr"${s}><is><t xml:space="preserve">${esc(text(v))}</t></is></c>`
  }
  const names = cols.map((_, j) => colName(j))
  const out = [
    `<row r="1">${cols.map((c, j) => cell(`${names[j]}1`, c, ' s="1"')).join('')}</row>`,
    ...rows.map((r, i) => `<row r="${i + 2}">${cols.map((c, j) => cell(`${names[j]}${i + 2}`, r[c])).join('')}</row>`),
  ]
  const ws = `${XML}<worksheet xmlns="${NS}/spreadsheetml/2006/main"><sheetViews><sheetView workbookViewId="0"><pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/></sheetView></sheetViews><sheetData>${out.join('')}</sheetData></worksheet>`
  const wb = `${XML}<workbook xmlns="${NS}/spreadsheetml/2006/main" xmlns:r="${NS}/officeDocument/2006/relationships"><sheets><sheet name="${esc(sheet)}" sheetId="1" r:id="rId1"/></sheets></workbook>`
  const files = { ...PARTS, 'xl/workbook.xml': wb, 'xl/worksheets/sheet1.xml': ws }
  return zipSync(Object.fromEntries(Object.entries(files).map(([k, v]) => [k, strToU8(v)])), { level: 6 })
}

const PARQUET_TYPE = { character: 'STRING', integer: 'INT32', double: 'DOUBLE', logical: 'BOOLEAN' }

/**
 * A Parquet file (snappy).
 * @param {Object<string, string>} types column -> R type (`character`,
 *   `integer`, `double`, `logical`); others are written as strings
 * @returns {Promise<ArrayBuffer>}
 */
export async function toParquet(cols, rows, types) {
  const { parquetWriteBuffer } = await import('hyparquet-writer')
  const columnData = cols.map((c) => {
    const type = PARQUET_TYPE[types[c]] ?? 'STRING'
    const conv = type === 'STRING' ? (v) => (v == null ? null : text(v)) : (v) => (v ?? null)
    return { name: c, type, data: rows.map((r) => conv(r[c])) }
  })
  return parquetWriteBuffer({ columnData, createdBy: 'TEPS course plan browser' })
}
