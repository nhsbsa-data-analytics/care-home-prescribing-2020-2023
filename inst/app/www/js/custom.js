$(document).ready(function () {
  // Whenever navigation tab button is clicked, window scrolls to the top
  $('#mainTabs a[data-toggle=\"tab\"]').on('click', function (e) {
    window.scrollTo(0, 0);
  });
  
  // Add new tab icon for external links and set to open in new tab
  var links = $("a[target!=_blank]");
  links.each(function(){
    // Current link
    var a = $(this);
    // Exit early for various types of link
    if (
      a.attr('href').match(/^mailto\:/)                   // Email links
        || a.attr('href').match(/^javascript\:/)          // Starting javascript
        || a.attr('href').match(/^#/)                     // Starting # (e.g. tabs)
        || a.attr('href').match(/^$/)                     // Empty links
        || a.attr('href').match(/^#maincontent$/)         // Skip link
        || (a.attr('class') &&                            // NHSBSA header icon
              a.attr('class').match(/^nhsuk-header__link$/))
    ) { return; }
    
    if (!(a[0].hostname === "localhost" || a[0].hostname === "127.0.0.1")) {
      // External link
      // Append space then icon to link
      a.after('&nbsp;<i class="fa-solid fa-arrow-up-right-from-square"></i>');
      // Force open in new tab
      a.attr('target', '_blank');
    } else {
      // Internal link
      var tabName = a.attr('href').split("/")[3].split("?")[0].replace(/_/g, ' ');
      var id = a.attr('href').split("?")[1] ?? '';
      
      a.removeAttr('href').
      attr('onclick', 'internalLink(\'' + tabName + '\', \'' + id + '\');');
    }
  });
  
  // Remove aria attributes from anchor tags to prevent accessibility issues
  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.type === "attributes") {
        mutation.target.removeAttribute("aria-selected");
      }
    })
  });
  
  // Not really necessary with just one flag, but may want to add further 
  // observer behaviour later
  var config = {attributes: true};

  // Add observers for all navigation links
  document.querySelectorAll('#mainTabs a[data-toggle=\"tab\"]').
    forEach(function(element){
      observer.observe(element, config);
    });
    
    
  // Handle clicks on mod_06 datatable, to outline selected map region
  Shiny.addCustomMessageHandler("rowClicked",
    function(message) {
      // First, remove border from currently bordered areas
      // previous_row may be null, so only update if it has a value
      if(Number.isInteger(message.previous_row)) {
        $("#geo_ch_flag-map_ch").highcharts().series[0]
          .points[message.previous_row].update({
            borderWidth: 0
        });
        
        $("#geo_ch_flag-map_non_ch").highcharts().series[0]
          .points[message.previous_row].update({
            borderWidth: 0
        });
      }
      
      // Second, add border to area corresponding with current selection
      $("#geo_ch_flag-map_ch").highcharts().series[0]
        .points[message.row].update({
          borderWidth: 1.25
      });
      
      $("#geo_ch_flag-map_non_ch").highcharts().series[0]
        .points[message.row].update({
          borderWidth: 1.25
      });
    }
  );
  
  
  // Sync footer with main table for mod_06
  const mainTableElementSelector = '#geo_ch_flag-main_table table';
  var resizeTimer;

  $(window).on('resize', function() {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(function() {
      if ($.fn.DataTable && $.fn.DataTable.isDataTable(mainTableElementSelector)) {
        $(mainTableElementSelector).dataTable().api().columns.adjust();
      }
      syncColumnWidths();
      syncScrolls();
    }, 150);
  });
});


// Go to specified tab and scroll position
var gotoTabPos = function(tab, pos) {
  $('#mainTabs a[data-value=\"' + tab + '\"]').click();
  $(window).scrollTop(pos);
  
  // reset tracking vars to null
  tabOnClick = null;
  posOnClick = null;
};

// global vars to track tab and position when internal link is clicked
var tabOnClick = null;
var posOnClick = null;

// Assign function to back browser button
window.onpopstate = function() {
   gotoTabPos(tabOnClick, posOnClick);
   
   // Replace current history state with current URL, to prevent back/forward
   // buttons affecting page until next time internal link is clicked
   setTimeout(function () {
     history.replaceState(null , '', window.location.href)
   }, 1000);
};


// Internal links - allows to navigate directly to a given tab and id
// Saves tab and page position when click occurs, to enable going back to same
var internalLink = function(tabName, id) {
  // Save current tab and position
  tabOnClick = $('#mainTabs li.active a').attr('data-value');
  posOnClick = $(window).scrollTop();
  
  // Initialise history to enable back button
  history.pushState({}, '');
  
  var tabList = document.getElementsByTagName('a');
  for (var i = 0; i < tabList.length; i++) {
    var tab = tabList[i];
    if(tab.getAttribute("data-value") == tabName) {
      tab.click();
      
      // check for target id, exit early if not present
      var $id = $('#' + id);
      if ($id.length === 0) {
          return;
      }
      
      document.getElementById(id).scrollIntoView({
        behavior: 'smooth'
      });
    };
  }
};


// Sync column widths between main and footer tables for mod_06
function syncColumnWidths() {
  setTimeout(function() {
    const mainTableSelector = '#geo_ch_flag-main_table';
    const footerTableSelector = '#geo_ch_flag-footer_table';
    
    const mainScrollBody = $(mainTableSelector + ' .dataTables_scrollBody');
    const footerScrollBody = $(footerTableSelector + ' .dataTables_scrollBody');
    const mainHeaderTable = $(mainTableSelector + ' .dataTables_scrollHeadInner table');
    const footerTableElement = $(footerTableSelector + ' table');
    const mainHeaders = $(mainTableSelector + ' .dataTables_scrollHead th');
    const footerCells = $(footerTableSelector + ' tr:has(td):first td');

    if (mainScrollBody.length === 0 || mainHeaders.length === 0) {
      return;
    }

    const correctContainerWidth = mainScrollBody.get(0).clientWidth;
    footerScrollBody.css('width', correctContainerWidth + 'px');
    
    const correctContentWidth = mainHeaderTable.outerWidth();
    footerTableElement.css('width', correctContentWidth + 'px');

    if (mainHeaders.length === footerCells.length) {
      mainHeaders.each(function(index) {
        const headerWidth = $(this).outerWidth();
        footerCells.eq(index).css({
          'box-sizing': 'border-box',
          'width': headerWidth + 'px'
        });
      });
    }
  }, 50);
}


// Sync scrolling between main and footer tables for mod_06
function syncScrolls() {
  const mainScrollBody = '#geo_ch_flag-main_table .dataTables_scrollBody';
  const footerScrollBody = '#geo_ch_flag-footer_table .dataTables_scrollBody';
  
  if ($(mainScrollBody).length > 0) {
    $(mainScrollBody).off('scroll.dtSync').on('scroll.dtSync', function() {
      $(footerScrollBody).scrollLeft($(this).scrollLeft());
    });
  }
}


// Handle click on footer links which behave as pages
Shiny.addCustomMessageHandler('switchTab', function(message) {
  $('a[data-value=\"' + message + '\"]').click();
});
