using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class WalkAnimControl : MonoBehaviour
{
    public Animator animator; // 指向游戏对象的Animator组件的引用
    private float timer = 0f; // 计时器
    private int currentState = 0; // 当前State的值

    public void PlayWalkAnim()
    {

            currentState = 0; // 这将在0和1之间切换
            animator.SetInteger("State", currentState);
    }
    public void PlayTurnAnim()
    {

        currentState = 1; // 这将在0和1之间切换
        animator.SetInteger("State", currentState);
    }
}