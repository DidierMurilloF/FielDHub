function fieldhubMetadataSheet(xlsx, config) {
  "use strict";
  var main = "http://schemas.openxmlformats.org/spreadsheetml/2006/main";
  var officeRels = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";
  var packageRels = "http://schemas.openxmlformats.org/package/2006/relationships";
  var contentTypes = "http://schemas.openxmlformats.org/package/2006/content-types";
  var workbook = xlsx.xl["workbook.xml"];
  var sheets = workbook.getElementsByTagNameNS(main, "sheets")[0];
  var relations = xlsx.xl._rels["workbook.xml.rels"];
  var types = xlsx["[Content_Types].xml"];
  var number = 1;
  while (Object.prototype.hasOwnProperty.call(xlsx.xl.worksheets, "sheet" + number + ".xml")) {
    number += 1;
  }
  var path = "worksheets/sheet" + number + ".xml";
  var sheetId = 1;
  var names = [];
  Array.prototype.forEach.call(sheets.getElementsByTagNameNS(main, "sheet"), function (sheet) {
    sheetId = Math.max(sheetId, Number(sheet.getAttribute("sheetId")) + 1);
    names.push(sheet.getAttribute("name"));
  });
  var name = "FielDHub metadata";
  var suffix = 2;
  while (names.indexOf(name) !== -1) {
    name = "FielDHub metadata " + suffix;
    suffix += 1;
  }
  var existing = [];
  Array.prototype.forEach.call(relations.getElementsByTagNameNS(packageRels, "Relationship"), function (rel) {
    existing.push(rel.getAttribute("Id"));
  });
  var relationId = "rIdFieldHub" + number;
  while (existing.indexOf(relationId) !== -1) relationId += "_";

  var document = workbook.implementation.createDocument(main, "worksheet", null);
  var columns = document.createElementNS(main, "cols");
  [8, 120].forEach(function (width, index) {
    var column = document.createElementNS(main, "col");
    column.setAttribute("min", index + 1);
    column.setAttribute("max", index + 1);
    column.setAttribute("width", width);
    column.setAttribute("customWidth", "1");
    columns.appendChild(column);
  });
  document.documentElement.appendChild(columns);
  var data = document.createElementNS(main, "sheetData");
  function addRow(values, index) {
    var row = document.createElementNS(main, "row");
    row.setAttribute("r", index);
    values.forEach(function (value, column) {
      var cell = document.createElementNS(main, "c");
      cell.setAttribute("r", (column === 0 ? "A" : "B") + index);
      cell.setAttribute("t", "inlineStr");
      var inline = document.createElementNS(main, "is");
      var text = document.createElementNS(main, "t");
      text.setAttributeNS("http://www.w3.org/XML/1998/namespace", "xml:space", "preserve");
      text.textContent = String(value);
      inline.appendChild(text);
      cell.appendChild(inline);
      row.appendChild(cell);
    });
    data.appendChild(row);
  }
  addRow(["PART", "TEXT (join parts without adding separators)"], 1);
  config.fieldhubMetadata.forEach(function (part, index) { addRow([index + 1, part], index + 2); });
  document.documentElement.appendChild(data);
  xlsx.xl.worksheets["sheet" + number + ".xml"] = document;

  var sheet = workbook.createElementNS(main, "sheet");
  sheet.setAttribute("name", name);
  sheet.setAttribute("sheetId", sheetId);
  sheet.setAttributeNS(officeRels, "r:id", relationId);
  sheets.appendChild(sheet);
  var relation = relations.createElementNS(packageRels, "Relationship");
  relation.setAttribute("Id", relationId);
  relation.setAttribute("Type", officeRels + "/worksheet");
  relation.setAttribute("Target", path);
  relations.documentElement.appendChild(relation);
  var type = types.createElementNS(contentTypes, "Override");
  type.setAttribute("PartName", "/xl/" + path);
  type.setAttribute("ContentType", "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml");
  types.documentElement.appendChild(type);
}
