$(document).ready(function () {
  // Whenever navigation tab button is clicked, window scrolls to the top
  $('#mainTabs a[data-toggle=\"tab\"]').on('click', function (e) {
    window.scrollTo(0, 0);
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
    
    
  // Add target='_blank' to external links so they open a new tab
  $(document.body).on('mouseover', 'a[target!=_blank]:not(.local)', function (e) {
    var a = $(this);
    if (
      !a.attr('href').match(/^mailto\:/)
         && (a[0].hostname != window.location.hostname)
         && !a.attr('href').match(/^javascript\:/)
         && !a.attr('href').match(/^$/)
    ) {
        a.attr('target', '_blank');
    } else {
        a.addClass('local');
    }
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
      var $id = $(id);
      if ($id.length === 0) {
          return;
      }
      
      document.getElementById(id).scrollIntoView({
        behavior: 'smooth'
      });
    };
  }
};
