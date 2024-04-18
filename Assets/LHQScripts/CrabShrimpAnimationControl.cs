using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace LevelArtInitialLHQ
{
    public class CrabShrimpAnimationControl : MonoBehaviour //脚本用于控制虾兵蟹将动画开始时间的随机Delay
    {
        private float timer;
        [SerializeField] private float DelayTime = 0.2f;
        private Animator anim;

        void Start()
        {
            anim = GetComponent<Animator>();
            if (anim)
            {
                anim.enabled = false;
            }
        }

        // Update is called once per frame
        void Update()
        {
            timer += Time.deltaTime;
            if (timer >= DelayTime)
            {
                timer = 0f;
                if (anim)
                {
                    anim.enabled = true;
                }
            }
        }
    }
}
