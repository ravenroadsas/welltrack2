// WellTrack 2.0 - client-side activity logger (process mining)
// Captures input changes and tab navigation in the browser and sends them to
// the server in batches (one message every FLUSH_MS) to keep the R process free.
(function () {
  var FLUSH_MS = 5000;
  var IGNORE = /^(wt_activity|.*_rows_current|.*_rows_all|.*_state|.*_search|.*_cell_clicked|.*_cells_selected|.*_columns_selected|.*_rows_selected_last|.*__shinyjs|.*_hover|.*_mouseover|.*_brush|.*_clicked_data|.*_clicked_serie|.*_clicked_row|.*_mouseover_.*|\.clientdata.*)$/;
  var buffer = { ts: [], input_id: [], value: [] };

  $(document).on('shiny:inputchanged', function (e) {
    if (!e.name || IGNORE.test(e.name)) return;
    var v = e.value;
    if (typeof v === 'object') { try { v = JSON.stringify(v); } catch (err) { v = ''; } }
    buffer.ts.push(Date.now());
    buffer.input_id.push(e.name);
    buffer.value.push(String(v === null || v === undefined ? '' : v).slice(0, 100));
  });

  setInterval(function () {
    if (!buffer.ts.length || !window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue('wt_activity', buffer, { priority: 'event' });
    buffer = { ts: [], input_id: [], value: [] };
  }, FLUSH_MS);
})();
