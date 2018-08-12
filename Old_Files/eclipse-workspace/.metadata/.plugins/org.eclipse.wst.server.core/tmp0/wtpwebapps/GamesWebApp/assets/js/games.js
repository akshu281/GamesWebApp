var searchVisible = 0;
var transparent = true;
var mobile_device = false;
var baseUrl = "/GamesWebApp/games";

$(document).ready(function () {
    initComponents();

    $.material.init();

    /*  Activate the tooltips      */
   // $('[rel="tooltip"]').tooltip();
    $(function () {
    	  $('[data-toggle="tooltip"]').tooltip(
              {container: 'body',
               // width: 900,
            }
          )
    	})

    // Code for the Validator
    var $validator = $('.wizard-card form').validate({
        rules: {
             optradio0: {
                  required: true
              },
              optradio: {
                  required: true
              },
             optradio1: {
                  required: true
              },
              optradio2: {
                  required: true
              },
              optradio3: {
                  required: true
              },
                optradio4: {
                     required: true
                },
                 optradio5: {
                     required: true
                 },
                 optradio6: {
                     required: true
                 },
                 optradio7: {
                     required: true
                 },
                 optradio8: {
                     required: true
                 },
                 optradio9: {
                     required: true
                 }
        },
        errorPlacement: function (error, element) {
            $(element).parent('div').addClass('has-error');
           // alert('Please select all the options')
        }
    });

    // Wizard Initialization
    $('.wizard-card').bootstrapWizard({
        'tabClass': 'nav nav-pills',
        'nextSelector': '.btn-next',
        'previousSelector': '.btn-previous',

        onNext: function (tab, navigation, index) {
            //var $valid = $('.wizard-card form').valid();
            var $valid = $('.wizard-card form').valid();
            if (!$valid) {
                $validator.focusInvalid();
                return false;
            }
            if (index == 1) {
                var optradio0 = $("#games input[name=optradio0]:checked").val();
                var optradio = $("#games input[name=optradio]:checked").val();
                var optradio1 = $("#games input[name=optradio1]:checked").val();
                var optradio2 = $("#games input[name=optradio2]:checked").val();
                var optradio3 = $("#games input[name=optradio3]:checked").val();
                var optradio4 = $("#games input[name=optradio4]:checked").val();
                var optradio5 = $("#games input[name=optradio5]:checked").val();
                var inputfacts = "User" + "," + optradio0 + "," + optradio + "," + optradio1 + "," + optradio2 + "," + optradio3;
                console.log(inputfacts);
                $.ajax({
                    url: baseUrl,
                    //url: '/GamesWebApp/User/assertfacts',
                    type: "POST",
                    data: {
                        action: "personality",
                        inputfacts: inputfacts,
                    },

                    success: function (result) {
                        console.log(result);
                        console.log('Success of Asserting Facts from Wiz 1');
                                        }
                
                }); 
            } 
                else if (index == 2) {
                var optradio4 = $("#games input[name=optradio4]:checked").val();
                var optradio5 = $("#games input[name=optradio5]:checked").val();
                var optradio6 = $("#games input[name=optradio6]:checked").val();
                var optradio7 = $("#games input[name=optradio7]:checked").val();
                var optradio8 = $("#games input[name=optradio8]:checked").val();
                var optradio9 = $("#games input[name=optradio9]:checked").val();
                var inputfacts1 = "User" + "," + optradio4 + "," + optradio5 + "," + optradio6 + "," + optradio7 + "," + optradio8 + "," + optradio9;
                console.log(inputfacts1);
                $.ajax({
                    url: baseUrl,
                    type: "POST",
                    data: {
                        action: "gametype",
                        inputfacts1: inputfacts1,
                    },

                    success: function (result) {
                        console.log(result);
                        console.log('Success of Asserting Facts from Wiz 2');
                              }

                });
            }
        },

        onInit: function (tab, navigation, index) {
            //check number of tabs and fill the entire row
            var $total = navigation.find('li').length;
            var $wizard = navigation.closest('.wizard-card');

            $first_li = navigation.find('li:first-child a').html();
            $moving_div = $('<div class="moving-tab">' + $first_li + '</div>');
            $('.wizard-card .wizard-navigation').append($moving_div);

            refreshAnimation($wizard, index);

            $('.moving-tab').css('transition', 'transform 0s');
        },

        onTabClick: function (tab, navigation, index) {
            var $valid = $('.wizard-card form').valid();

            if (!$valid) {
                return false;
            } else {
                return true;
            }
        },

        onTabShow: function (tab, navigation, index) {
            var $total = navigation.find('li').length;
            var $current = index + 1;

            var $wizard = navigation.closest('.wizard-card');

            // If it's the last tab then hide the last button and show the finish instead
            if ($current >= $total) {
                $($wizard).find('.btn-next').hide();
                $($wizard).find('.btn-finish').show();
            } else {
                $($wizard).find('.btn-next').show();
                $($wizard).find('.btn-finish').hide();
            }

            button_text = navigation.find('li:nth-child(' + $current + ') a').html();

            setTimeout(function () {
                $('.moving-tab').text(button_text);
            }, 150);

            var checkbox = $('.footer-checkbox');

            if (!index == 0) {
                $(checkbox).css({
                    'opacity': '0',
                    'visibility': 'hidden',
                    'position': 'absolute'
                });
            } else {
                $(checkbox).css({
                    'opacity': '1',
                    'visibility': 'visible'
                });
            }

            refreshAnimation($wizard, index);
        }
    });

    // Retrieve the values from Clips

    function initComponents() {
        $("#finish-button").click(function () {
            console.log("inside finish func");
            $.ajax({
                url: baseUrl,
                type: "GET",
                data: {
                    action: "finish",
                },
                success: function (result) {
                    console.log(result);
                    $("#games textarea[name=resultarea]").val(result);
                }
            });
        });
    }


    $('[data-toggle="wizard-radio"]').click(function () {
        wizard = $(this).closest('.wizard-card');
        wizard.find('[data-toggle="wizard-radio"]').removeClass('active');
        $(this).addClass('active');
        $(wizard).find('[type="radio"]').removeAttr('checked');
        $(this).find('[type="radio"]').attr('checked', 'true');
    });

    $('[data-toggle="wizard-checkbox"]').click(function () {
        if ($(this).hasClass('active')) {
            $(this).removeClass('active');
            $(this).find('[type="checkbox"]').removeAttr('checked');
        } else {
            $(this).addClass('active');
            $(this).find('[type="checkbox"]').attr('checked', 'true');
        }
    });

    $('.set-full-height').css('height', 'auto');

});


$(window).resize(function () {
    $('.wizard-card').each(function () {
        $wizard = $(this);

        index = $wizard.bootstrapWizard('currentIndex');
        refreshAnimation($wizard, index);

        $('.moving-tab').css({
            'transition': 'transform 0s'
        });
    });
});

function refreshAnimation($wizard, index) {
    $total = $wizard.find('.nav li').length;
    $li_width = 100 / $total;

    total_steps = $wizard.find('.nav li').length;
    move_distance = $wizard.width() / total_steps;
    index_temp = index;
    vertical_level = 0;

    mobile_device = $(document).width() < 600 && $total > 3;

    if (mobile_device) {
        move_distance = $wizard.width() / 2;
        index_temp = index % 2;
        $li_width = 50;
    }

    $wizard.find('.nav li').css('width', $li_width + '%');

    step_width = move_distance;
    move_distance = move_distance * index_temp;

    $current = index + 1;

    if ($current == 1 || (mobile_device == true && (index % 2 == 0))) {
        move_distance -= 8;
    } else if ($current == total_steps || (mobile_device == true && (index % 2 == 1))) {
        move_distance += 8;
    }

    if (mobile_device) {
        vertical_level = parseInt(index / 2);
        vertical_level = vertical_level * 38;
    }

    $wizard.find('.moving-tab').css('width', step_width);
    $('.moving-tab').css({
        'transform': 'translate3d(' + move_distance + 'px, ' + vertical_level + 'px, 0)',
        'transition': 'all 0.5s cubic-bezier(0.29, 1.42, 0.79, 1)'

    });
}

materialDesign = {

    checkScrollForTransparentNavbar: debounce(function () {
        if ($(document).scrollTop() > 260) {
            if (transparent) {
                transparent = false;
                $('.navbar-color-on-scroll').removeClass('navbar-transparent');
            }
        } else {
            if (!transparent) {
                transparent = true;
                $('.navbar-color-on-scroll').addClass('navbar-transparent');
            }
        }
    }, 17)

}

function debounce(func, wait, immediate) {
    var timeout;
    return function () {
        var context = this,
            args = arguments;
        clearTimeout(timeout);
        timeout = setTimeout(function () {
            timeout = null;
            if (!immediate) func.apply(context, args);
        }, wait);
        if (immediate && !timeout) func.apply(context, args);
    };
};